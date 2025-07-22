package net.loveruby.cflat.sysdep.arm64;

import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.ir.*;
import net.loveruby.cflat.type.*;
import net.loveruby.cflat.utils.ErrorHandler;
import java.util.*;

public class CodeGenerator implements net.loveruby.cflat.sysdep.CodeGenerator, IRVisitor<Void, Void> {
    private AssemblyCode assembly;
    private ErrorHandler errorHandler;
    private net.loveruby.cflat.asm.Type naturalType;
    private net.loveruby.cflat.sysdep.CodeGeneratorOptions options;

    // ARM64 calling convention
    private static final Register[] ARG_REGS = { Register.X0, Register.X1, Register.X2, Register.X3,
            Register.X4, Register.X5, Register.X6, Register.X7 };
    private static final Register RETURN_REG = Register.X0;
    private static final Register STACK_POINTER = Register.SP;
    private static final Register FRAME_POINTER = Register.FP;
    private static final Register LINK_REGISTER = Register.LR;

    // Symbol tables
    private SymbolTable constSymbols;
    private SymbolTable labelSymbols;

    // Current function info
    private DefinedFunction currentFunction;
    private long stackSize;
    private Map<Entity, Long> localVarOffsets;
    private Map<Entity, Long> paramOffsets;

    // 简化的寄存器管理 - 只保留基本字段，不再使用复杂的分配策略
    private Set<Register> usedRegisters;

    public CodeGenerator(net.loveruby.cflat.sysdep.CodeGeneratorOptions opts,
            net.loveruby.cflat.asm.Type naturalType, ErrorHandler h) {
        this.options = opts;
        this.naturalType = naturalType;
        this.errorHandler = h;
        this.assembly = new AssemblyCode(h);
        this.constSymbols = new SymbolTable(".LC");
        this.labelSymbols = new SymbolTable(""); // Use empty base for numeric labels
        this.localVarOffsets = new HashMap<>();
        this.paramOffsets = new HashMap<>();
        this.usedRegisters = new HashSet<>();
    }

    public AssemblyCode generate(net.loveruby.cflat.ir.IR ir) {
        // Generate data section for constants
        generateDataSection(ir);

        // Generate text section for functions
        generateTextSection(ir);

        return assembly;
    }

    private void generateDataSection(net.loveruby.cflat.ir.IR ir) {
        // Generate string literals
        for (ConstantEntry ent : ir.constantTable().entries()) {
            Symbol sym = constSymbols.newSymbol();
            ent.setSymbol(sym);
            ent.setMemref(mem(sym));
            ent.setAddress(imm(sym));

            assembly.add(new Directive("\t.section\t__TEXT,__cstring,cstring_literals"));
            assembly.add(new Label(sym));
            assembly.add(new Directive("\t.asciz\t\"" + escapeString(ent.value()) + "\""));
        }

        // Generate global variables
        for (DefinedVariable var : ir.definedGlobalVariables()) {
            Symbol sym = new NamedSymbol(var.name());
            var.setMemref(mem(sym));
            var.setAddress(imm(sym));

            assembly.add(new Directive("\t.section\t__DATA,__data"));
            assembly.add(new Label(sym));
            if (var.hasInitializer()) {
                // Handle initializers
                if (var.initializer() instanceof net.loveruby.cflat.ast.IntegerLiteralNode) {
                    net.loveruby.cflat.ast.IntegerLiteralNode init = (net.loveruby.cflat.ast.IntegerLiteralNode) var
                            .initializer();
                    long value = init.value();
                    assembly.add(new Directive("\t.quad\t" + value));
                } else {
                    // For now, just use 0 for other types
                    assembly.add(new Directive("\t.quad\t0"));
                }
            } else {
                assembly.add(new Directive("\t.space\t" + var.type().size()));
            }
        }

        // Generate common symbols (uninitialized global variables)
        for (DefinedVariable var : ir.definedCommonSymbols()) {
            Symbol sym = new NamedSymbol(var.name());
            var.setMemref(mem(sym));
            var.setAddress(imm(sym));

            assembly.add(new Directive("\t.section\t__DATA,__common"));
            assembly.add(new Label(sym));
            assembly.add(new Directive("\t.space\t" + var.type().size()));
        }
    }

    private void generateTextSection(net.loveruby.cflat.ir.IR ir) {
        assembly.add(new Directive("\t.section\t__TEXT,__text,regular,pure_instructions"));
        assembly.add(new Directive("\t.build_version macos, 11, 0"));

        for (DefinedFunction func : ir.definedFunctions()) {
            generateFunction(func);
        }
    }

    private void generateFunction(DefinedFunction func) {
        currentFunction = func;
        calculateStackLayout(func);

        // Reset register allocation for each function
        usedRegisters.clear();

        // Set up function symbols
        Symbol funcSym = new NamedSymbol("_" + func.name());
        func.setCallingSymbol(funcSym);
        func.setMemref(mem(funcSym));
        func.setAddress(imm(funcSym));

        // Function label
        assembly.add(new Directive("\t.globl\t_" + func.name()));
        assembly.add(new Directive("\t.p2align\t2"));
        assembly.add(new Label(funcSym));

        // Function prologue
        generatePrologue(func);

        // Generate function body
        for (Stmt stmt : func.ir()) {
            stmt.accept(this);
        }

        // Add epilogue label
        assembly.add(new Label(new NamedSymbol(".L" + func.name() + "_epilogue")));

        // Function epilogue
        generateEpilogue(func);
    }

    private void calculateStackLayout(DefinedFunction func) {
        /* ---------- 形参偏移：+16 起，每 8 字节 ---------- */
        List<Parameter> params = func.parameters();
        for (int i = 0; i < params.size(); i++) {
            paramOffsets.put(params.get(i), 16L + i * 8);
        }

        /* ---------- 统计局部变量所需大小 (向 8 对齐) ---------- */
        List<DefinedVariable> all = func.lvarScope().allVariablesWithPrivate();
        all.sort((a, b) -> a.name().compareTo(b.name()));
        long local = 0;
        for (DefinedVariable v : all) {
            if (v.isParameter())
                continue;
            local += ((v.type().allocSize() + 7) & ~7);
        }

        /* ---------- 总栈大小：16(FP/LR)+形参备份+locals，16 对齐 ---------- */
        stackSize = 16 + params.size() * 8 + local;
        stackSize = (stackSize + 15) & ~15;

        /* ---------- 为局部变量分配负偏移 ---------- */
        long cur = -8;
        for (DefinedVariable v : all) {
            if (v.isParameter())
                continue;
            localVarOffsets.put(v, cur);
            cur -= ((v.type().allocSize() + 7) & ~7);
        }
    }

    private void generatePrologue(DefinedFunction func) {
        assembly.add(new Directive("\tsub\tsp, sp, #" + stackSize));
        assembly.add(new Directive("\tstp\tx29, x30, [sp, #0]")); // 保存旧 FP/LR
        assembly.add(new Directive("\tadd\tx29, sp, #0")); // FP 指向帧底

        /* ---------- 备份形参 ---------- */
        List<Parameter> ps = func.parameters();
        for (int i = 0; i < ps.size() && i < ARG_REGS.length; i++) {
            assembly.add(new Directive(
                    "\tstr\t" + ARG_REGS[i] + ", [x29, #" + (16 + i * 8) + "]"));
        }
    }

    private void generateEpilogue(DefinedFunction func) {
        assembly.add(new Directive("\tldp\tx29, x30, [sp, #0]"));
        assembly.add(new Directive("\tadd\tsp, sp, #" + stackSize));
        assembly.add(new Directive("\tret"));
    }

    // IRVisitor implementations
    public Void visit(ExprStmt stmt) {
        stmt.expr().accept(this);
        return null;
    }

    public Void visit(Assign stmt) {
        // 加载右值到X0
        stmt.rhs().accept(this);
        assembly.add(new Directive("\tmov\tx1, x0"));

        // 处理左值
        if (stmt.lhs().isVar()) {
            Var var = (Var) stmt.lhs();
            Entity entity = var.entity();

            if (localVarOffsets.containsKey(entity)) {
                // Local variable - 使用帧指针相对寻址
                long offset = localVarOffsets.get(entity);
                if (var.type().size() == 8) {
                    assembly.add(new Directive("\tstur\tx1, [x29, #" + offset + "]"));
                } else {
                    assembly.add(new Directive("\tstur\tw1, [x29, #" + offset + "]"));
                }
            } else if (paramOffsets.containsKey(entity)) {
                // Parameter
                long offset = paramOffsets.get(entity);
                assembly.add(new Directive("\tstur\tx1, [x29, #" + offset + "]"));
            } else {
                // Global variable
                assembly.add(new Directive("\tadrp\tx0, " + entity.name() + "@PAGE"));
                assembly.add(new Directive("\tadd\tx0, x0, " + entity.name() + "@PAGEOFF"));
                assembly.add(new Directive("\tstr\tx1, [x0]"));
            }
        } else if (stmt.lhs().isAddr()) {
            // Address expression - evaluate address into X0, then store
            Addr addr = (Addr) stmt.lhs();
            Entity entity = addr.entity();

            if (localVarOffsets.containsKey(entity)) {
                // Local variable - 直接存储右值的地址
                long offset = localVarOffsets.get(entity);
                assembly.add(new Directive("\tstur\tx1, [x29, #" + offset + "]")); // 存储到ptr的位置
            } else if (paramOffsets.containsKey(entity)) {
                // Parameter - 直接存储右值的地址
                long offset = paramOffsets.get(entity);
                assembly.add(new Directive("\tstur\tx1, [x29, #" + offset + "]")); // 存储到ptr的位置
            } else {
                // Global variable - 直接存储右值的地址
                assembly.add(new Directive("\tadrp\tx0, " + entity.name() + "@PAGE"));
                assembly.add(new Directive("\tadd\tx0, x0, " + entity.name() + "@PAGEOFF"));
                assembly.add(new Directive("\tstr\tx1, [x0]")); // 存储到ptr的位置
            }
        } else {
            // Complex left-hand side (e.g., array access, pointer dereference)
            // 计算左值地址到X0
            if (stmt.lhs() instanceof Mem) {
                Mem mem = (Mem) stmt.lhs();
                // 检查是否是嵌套Mem（如*ptrs[0]）
                if (mem.expr() instanceof Mem) {
                    // 嵌套Mem：先计算内层Mem的地址，再加载指针值
                    Mem innerMem = (Mem) mem.expr();
                    innerMem.expr().accept(this); // 计算内层地址到x0
                    assembly.add(new Directive("\tmov\tx2, x0")); // 保存地址到x2
                    assembly.add(new Directive("\tldr\tx0, [x2]")); // 加载指针值到x0
                } else {
                    // 简单Mem：直接使用mem.expr()的结果作为地址
                    mem.expr().accept(this); // 计算地址到x0
                }
            } else {
                stmt.lhs().accept(this);
            }

            // 存储右值到计算出的地址
            assembly.add(new Directive("\tstr\tx1, [x0]"));
        }

        return null;
    }

    public Void visit(CJump stmt) {
        // Load condition into X0
        stmt.cond().accept(this);

        // Compare with zero
        // For ARM64, conditional branches can only jump to local labels (numeric
        // labels)
        // We need to use a different approach: use unconditional branch with condition
        assembly.add(new Directive("\tcmp\tx0, #0"));
        assembly.add(new Directive("\tb.ne\t" + stmt.thenLabel().symbol().toSource(labelSymbols) + "f"));
        assembly.add(new Directive("\tb\t" + stmt.elseLabel().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    public Void visit(Jump stmt) {
        assembly.add(new Directive("\tb\t" + stmt.label().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    public Void visit(Switch stmt) {
        // Load switch value into X0
        stmt.cond().accept(this);

        // TODO: Implement switch statement
        // For now, just jump to default
        assembly.add(new Directive("\tb\t" + stmt.defaultLabel().symbol()));
        return null;
    }

    public Void visit(LabelStmt stmt) {
        // For ARM64, we need to generate local labels in the format "0:", "1:", etc.
        String labelName = stmt.label().symbol().toSource(labelSymbols);
        assembly.add(new Directive(labelName + ":"));
        return null;
    }

    public Void visit(Return stmt) {
        if (stmt.expr() != null) {
            stmt.expr().accept(this);
            // Return value is already in X0
        } else {
            // Return 0
            assembly.add(new Directive("\tmov\tx0, #0"));
        }

        // Jump to epilogue
        assembly.add(new Directive("\tb\t.L" + currentFunction.name() + "_epilogue"));
        return null;
    }

    public Void visit(Uni expr) {
        // Load operand into X0
        expr.expr().accept(this);

        switch (expr.op()) {
            case UMINUS:
                assembly.add(new Directive("\tneg\tx0, x0"));
                break;
            case BIT_NOT:
                // For 32-bit values, use 32-bit operations and sign extend
                if (expr.type().size() == 4) {
                    // 32-bit bitwise NOT
                    assembly.add(new Directive("\tmvn\tw0, w0"));
                    assembly.add(new Directive("\tsxtw\tx0, w0")); // Sign extend to 64-bit
                } else {
                    // 64-bit bitwise NOT
                    assembly.add(new Directive("\tmvn\tx0, x0"));
                }
                break;
            case NOT:
                assembly.add(new Directive("\tcmp\tx0, #0"));
                assembly.add(new Directive("\tcset\tx0, eq"));
                break;
            case S_CAST:
                // Sign extend from source type to destination type
                if (expr.expr().type().size() == 4 && expr.type().size() == 8) {
                    // 32-bit to 64-bit sign extension
                    // visit(Var expr)已经使用了uxtw，不需要再次扩展
                    // Do nothing, value is already in the right format
                } else if (expr.expr().type().size() == 8 && expr.type().size() == 8) {
                    // 64-bit to 64-bit, no extension needed
                    // Do nothing, value is already in the right format
                }
                // For other cases, no action needed as the value is already in the right format
                break;
            case U_CAST:
                // Zero extend from source type to destination type
                if (expr.expr().type().size() == 4 && expr.type().size() == 8) {
                    // 32-bit to 64-bit zero extension
                    assembly.add(new Directive("\tuxtw\tx0, w0"));
                }
                // For other cases, no action needed as the value is already in the right format
                break;
        }
        return null;
    }

    public Void visit(Bin expr) {
        // Load left operand into X0
        expr.left().accept(this);
        // Save left operand to X1
        assembly.add(new Directive("\tmov\tx1, x0"));

        // Load right operand into X0
        expr.right().accept(this);
        assembly.add(new Directive("\tmov\tx2, x0"));

        // Perform operation using X1 and X2, result in X0
        switch (expr.op()) {
            case ADD:
                assembly.add(new Directive("\tadd\tx0, x1, x2"));
                break;
            case SUB:
                assembly.add(new Directive("\tsub\tx0, x1, x2"));
                break;
            case MUL:
                assembly.add(new Directive("\tmul\tx0, x1, x2"));
                break;
            case S_DIV:
                assembly.add(new Directive("\tsdiv\tx0, x1, x2"));
                break;
            case S_MOD:
                assembly.add(new Directive("\tsdiv\tx3, x1, x2"));
                assembly.add(new Directive("\tmul\tx3, x3, x2"));
                assembly.add(new Directive("\tsub\tx0, x1, x3"));
                break;
            case BIT_AND:
                assembly.add(new Directive("\tand\tx0, x1, x2"));
                break;
            case BIT_OR:
                assembly.add(new Directive("\torr\tx0, x1, x2"));
                break;
            case BIT_XOR:
                assembly.add(new Directive("\teor\tx0, x1, x2"));
                break;
            case BIT_LSHIFT:
                assembly.add(new Directive("\tlsl\tx0, x1, x2"));
                break;
            case ARITH_RSHIFT:
                assembly.add(new Directive("\tasr\tx0, x1, x2"));
                break;
            case EQ:
                assembly.add(new Directive("\tcmp\tx1, x2"));
                assembly.add(new Directive("\tcset\tx0, eq"));
                break;
            case NEQ:
                assembly.add(new Directive("\tcmp\tx1, x2"));
                assembly.add(new Directive("\tcset\tx0, ne"));
                break;
            case S_LT:
                assembly.add(new Directive("\tcmp\tx1, x2"));
                assembly.add(new Directive("\tcset\tx0, lt"));
                break;
            case S_LTEQ:
                assembly.add(new Directive("\tcmp\tx1, x2"));
                assembly.add(new Directive("\tcset\tx0, le"));
                break;
            case S_GT:
                assembly.add(new Directive("\tcmp\tx1, x2"));
                assembly.add(new Directive("\tcset\tx0, gt"));
                break;
            case S_GTEQ:
                assembly.add(new Directive("\tcmp\tx1, x2"));
                assembly.add(new Directive("\tcset\tx0, ge"));
                break;
            default:
                // For now, handle other cases with basic operations
                assembly.add(new Directive("\tmov\tx0, x1"));
                break;
        }
        return null;
    }

    public Void visit(Call expr) {
        List<Expr> args = expr.args();

        /* ---------- 非变长调用：逆序求值，直接放寄存器 ---------- */
        int n = Math.min(args.size(), ARG_REGS.length);
        for (int i = n - 1; i >= 0; i--) {
            args.get(i).accept(this); // 结果 -> x0
            if (i != 0)
                assembly.add(new Directive("\tmov\t" + ARG_REGS[i] + ", x0"));
        }

        /* ---------- 发起调用 ---------- */
        if (expr.isStaticCall())
            assembly.add(new Directive("\tbl\t_" + expr.function().name()));
        else {
            expr.expr().accept(this); // 函数指针 -> x0
            assembly.add(new Directive("\tblr\tx0"));
        }
        return null;
    }

    public Void visit(Addr expr) {
        Entity entity = expr.entity();
        if (localVarOffsets.containsKey(entity)) {
            // Local variable address - 使用帧指针相对寻址
            long offset = localVarOffsets.get(entity);
            assembly.add(new Directive("\tadd\tx0, x29, #" + offset));
        } else if (paramOffsets.containsKey(entity)) {
            // Parameter address
            long offset = paramOffsets.get(entity);
            assembly.add(new Directive("\tadd\tx0, x29, #" + offset));
        } else {
            // Global variable address
            assembly.add(new Directive("\tadrp\tx0, " + entity.name() + "@PAGE"));
            assembly.add(new Directive("\tadd\tx0, x0, " + entity.name() + "@PAGEOFF"));
        }
        return null;
    }

    public Void visit(Mem expr) {
        // Load address into X1
        expr.expr().accept(this);
        assembly.add(new Directive("\tmov\tx1, x0"));

        // Load from memory - 根据类型选择正确的加载指令
        if (expr.type().size() == 1) {
            // 8位值（char）
            assembly.add(new Directive("\tldrb\tw0, [x1]"));
            assembly.add(new Directive("\tsxtb\tx0, w0")); // 符号扩展到64位
        } else if (expr.type().size() == 4) {
            // 32位值（int）- 使用sxtw确保正确扩展
            assembly.add(new Directive("\tldr\tw0, [x1]"));
            assembly.add(new Directive("\tsxtw\tx0, w0"));
        } else {
            // 64位值（指针或long）
            assembly.add(new Directive("\tldr\tx0, [x1]"));
        }
        return null;
    }

    public Void visit(Var expr) {
        Entity ent = expr.entity();

        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (expr.type().size() == 8)
                assembly.add(new Directive("\tldr\tx0, [x29, #" + off + "]"));
            else {
                assembly.add(new Directive("\tldr\tw0, [x29, #" + off + "]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            }
        } else if (paramOffsets.containsKey(ent)) { // 形参：正偏移
            long off = paramOffsets.get(ent);
            if (expr.type().size() == 8)
                assembly.add(new Directive("\tldr\tx0, [x29, #" + off + "]"));
            else {
                assembly.add(new Directive("\tldr\tw0, [x29, #" + off + "]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            }
        } else {
            // Global variable
            assembly.add(new Directive("\tadrp\tx0, " + ent.name() + "@PAGE"));
            assembly.add(new Directive("\tadd\tx0, x0, " + ent.name() + "@PAGEOFF"));
            assembly.add(new Directive("\tldr\tx0, [x0]"));
        }
        return null;
    }

    public Void visit(Int expr) {
        long v = expr.value();

        /* ─── 1. 直接用汇编器接受的有符号立即数 ─── */
        if (v >= -0x8000 && v <= 0x7FFF) { // ±32767
            assembly.add(new Directive("\tmov\tx0, #" + v)); // 正负都行
            return null;
        }

        /* ─── 2. 绝对值按 16-bit block 拼 mov{n}/movk ─── */
        if (v < 0) {
            long uv = ~(-v) & 0xFFFFFFFFFFFFFFFFL; // 转成 movn 可接受形式
            assembly.add(new Directive("\tmovn\tx0, #" + (uv & 0xFFFF)));
            int shift = 16;
            while (shift < 64 && ((uv >> shift) != 0xFFFF)) {
                assembly.add(new Directive(
                        "\tmovk\tx0, #" + ((uv >> shift) & 0xFFFF) + ", lsl #" + shift));
                shift += 16;
            }
        } else {
            assembly.add(new Directive("\tmov\tx0, #" + (v & 0xFFFF)));
            for (int s = 16; s < 64; s += 16) {
                if ((v >> s) == 0)
                    break;
                assembly.add(new Directive(
                        "\tmovk\tx0, #" + ((v >> s) & 0xFFFF) + ", lsl #" + s));
            }
        }
        return null;
    }

    public Void visit(Str expr) {
        // Load string address
        assembly.add(new Directive("\tadrp\tx0, " + expr.entry().symbol() + "@PAGE"));
        assembly.add(new Directive("\tadd\tx0, x0, " + expr.entry().symbol() + "@PAGEOFF"));
        return null;
    }

    public AssemblyCode assembly() {
        return assembly;
    }

    // Helper methods
    private MemoryReference mem(Symbol sym) {
        return new DirectMemoryReference(sym);
    }

    private ImmediateValue imm(Symbol sym) {
        return new ImmediateValue(sym);
    }

    private String escapeString(String str) {
        return str.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n");
    }

}