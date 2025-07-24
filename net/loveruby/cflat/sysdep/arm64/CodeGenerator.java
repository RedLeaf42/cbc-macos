package net.loveruby.cflat.sysdep.arm64;

import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.ir.*;
import net.loveruby.cflat.type.FunctionType;
import net.loveruby.cflat.utils.ErrorHandler;

import java.util.*;

/**
 * ARM64 CodeGenerator for macOS (Apple ABI)
 * - Variadic call: 64B shadow space for vararg tail only
 * - Mach-O sections/visibility
 * - Static locals emitted to data, not stack
 * - Prologue/Epilogue use push/pop pattern to avoid stp/ldp offset overflow
 * - Address calculation no longer trashes x0 in the middle
 */
public class CodeGenerator implements net.loveruby.cflat.sysdep.CodeGenerator, IRVisitor<Void, Void> {

    /* ====== Config ====== */
    private final AssemblyCode assembly;
    private final ErrorHandler errorHandler;
    private final net.loveruby.cflat.asm.Type naturalType;
    private final net.loveruby.cflat.sysdep.CodeGeneratorOptions options;

    private static final Register[] ARG_REGS = {
            Register.X0, Register.X1, Register.X2, Register.X3,
            Register.X4, Register.X5, Register.X6, Register.X7
    };
    private static final Register VAL_TMP = Register.X9; // 仅用于保存值(RHS等)
    private static final Register ADR_TMP0 = Register.X11; // 地址计算用
    private static final Register ADR_TMP1 = Register.X12; // 地址计算用
    private static final Register SCRATCH = Register.X9; // general scratch in call setup

    private final SymbolTable constSymbols = new SymbolTable(".LC");
    private final SymbolTable labelSymbols = new SymbolTable("");

    // per-function context
    private DefinedFunction currentFunction;
    private long frameSize; // size we sub after pushing FP/LR
    private final Map<Entity, Long> localVarOffsets = new HashMap<>();
    private final Map<Entity, Long> paramOffsets = new HashMap<>();
    private final List<DefinedVariable> staticLocals = new ArrayList<>();
    private static final Register TMP1 = Register.X10;
    private static final Register TMP0 = Register.X13; // 使用不同的寄存器避免冲突
    private static final Register CALL_TMP = Register.X16;
    private static final Register CALL_TMP2 = Register.X17; // IP1 (如果还需要第二个)

    public CodeGenerator(net.loveruby.cflat.sysdep.CodeGeneratorOptions opts,
            net.loveruby.cflat.asm.Type naturalType,
            ErrorHandler h) {
        this.options = opts;
        this.naturalType = naturalType;
        this.errorHandler = h;
        this.assembly = new AssemblyCode(h);
    }

    /* ====== Entry ====== */
    @Override
    public AssemblyCode generate(IR ir) {
        collectStaticLocals(ir);
        generateDataSection(ir);
        generateTextSection(ir);
        return assembly;
    }

    private void collectStaticLocals(IR ir) {
        staticLocals.clear();
        for (DefinedFunction f : ir.definedFunctions()) {
            for (DefinedVariable v : f.lvarScope().allVariablesWithPrivate()) {
                if (!v.isParameter() && v.isPrivate()) {
                    staticLocals.add(v);
                }
            }
        }
    }

    /* ====== Data ====== */
    private void generateDataSection(IR ir) {
        // 1. string literals
        for (ConstantEntry ent : ir.constantTable().entries()) {
            Symbol sym = constSymbols.newSymbol();
            ent.setSymbol(sym);
            ent.setMemref(mem(sym));
            ent.setAddress(imm(sym));
            assembly.add(new Directive("\t.section\t__TEXT,__cstring,cstring_literals"));
            assembly.add(new Label(sym));
            assembly.add(new Directive("\t.asciz\t\"" + escapeString(ent.value()) + "\""));
        }

        // 2. initialized extern globals
        for (DefinedVariable var : ir.definedGlobalVariables()) {
            if (var.isPrivate())
                continue;
            emitInitializedGlobal(var, true, ir);
        }
        // 3. static locals (private in IR)
        for (DefinedVariable var : staticLocals) {
            emitInitializedGlobal(var, false, ir);
        }
        // 4. common (tentative) symbols
        for (DefinedVariable var : ir.definedCommonSymbols()) {
            emitCommonSymbol(var);
        }
    }

    private void emitInitializedGlobal(DefinedVariable var, boolean external, IR ir) {
        Symbol sym = new NamedSymbol(var.name());
        var.setMemref(mem(sym));
        var.setAddress(imm(sym));

        assembly.add(new Directive("\t.section\t__DATA,__data"));
        if (external) {
            assembly.add(new Directive("\t.globl\t_" + var.name()));
        } else {
            assembly.add(new Directive("\t.private_extern\t_" + var.name()));
        }
        assembly.add(new Label(sym));

        if (var.hasInitializer()) {
            Object init = var.initializer();
            if (init instanceof net.loveruby.cflat.ast.IntegerLiteralNode) {
                long v = ((net.loveruby.cflat.ast.IntegerLiteralNode) init).value();
                assembly.add(new Directive("\t.quad\t" + v));
            } else if (init instanceof net.loveruby.cflat.ast.StringLiteralNode) {
                String s = ((net.loveruby.cflat.ast.StringLiteralNode) init).value();
                ConstantEntry ce = ir.constantTable().intern(s);
                assembly.add(new Directive("\t.quad\t" + ce.symbol().toSource()));
            } else if (init instanceof net.loveruby.cflat.ir.Str) {
                Symbol cs = ((net.loveruby.cflat.ir.Str) init).entry().symbol();
                assembly.add(new Directive("\t.quad\t" + cs.toSource()));
            } else {
                assembly.add(new Directive("\t.quad\t0"));
            }
        } else {
            assembly.add(new Directive("\t.space\t" + var.type().size()));
        }
    }

    private void emitCommonSymbol(DefinedVariable var) {
        long size = var.type().size();
        int align = Math.max(3, log2ceil(size)); // >= 8 byte

        Symbol sym = new NamedSymbol(var.name());
        var.setMemref(mem(sym));
        var.setAddress(imm(sym));

        assembly.add(new Directive("\t.globl\t_" + var.name()));
        assembly.add(new Directive("\t.zerofill\t__DATA,__common,_" + var.name() + "," + size + "," + align));
    }

    private int log2ceil(long n) {
        int e = 0;
        long v = 1;
        while (v < n) {
            v <<= 1;
            e++;
        }
        return e;
    }

    /* ====== Text ====== */
    private void generateTextSection(IR ir) {
        assembly.add(new Directive("\t.section\t__TEXT,__text,regular,pure_instructions"));
        assembly.add(new Directive("\t.build_version macos, 11, 0"));
        for (DefinedFunction f : ir.definedFunctions()) {
            generateFunction(f);
        }
    }

    private void generateFunction(DefinedFunction func) {
        currentFunction = func;
        localVarOffsets.clear();
        paramOffsets.clear();

        calcFrameLayout(func);

        Symbol fn = new NamedSymbol("_" + func.name());
        func.setCallingSymbol(fn);
        func.setMemref(mem(fn));
        func.setAddress(imm(fn));

        assembly.add(new Directive("\t.globl\t_" + func.name()));
        assembly.add(new Directive("\t.p2align\t2"));
        assembly.add(new Label(fn));

        genPrologue();
        for (Stmt s : func.ir())
            s.accept(this);
        assembly.add(new Label(new NamedSymbol(".L" + func.name() + "_epilogue")));
        genEpilogue();
    }

    /**
     * frameSize 不包含 FP/LR 16B；那 16B 是额外 push 的
     * 正确的ARM64栈帧布局：
     * [x29, #+16] - 参数1
     * [x29, #+24] - 参数2
     * [x29, #+0] - 保存的FP
     * [x29, #+8] - 返回地址
     * [x29, #-8] - 局部变量1
     * [x29, #-16] - 局部变量2
     */
    private void calcFrameLayout(DefinedFunction f) {
        // 局部变量从负偏移开始，从-8开始
        long localSize = 0;
        for (DefinedVariable v : f.lvarScope().allVariablesWithPrivate()) {
            if (v.isParameter()) {
                continue;
            }
            if (v.isPrivate()) {
                continue; // static local -> data
            }
            long sz = (v.type().allocSize() + 7) & ~7;
            long offset = -8 - localSize; // 负数偏移
            localVarOffsets.put(v, offset);
            localSize += sz;
        }

        // 参数从正偏移开始，从+16开始（跳过保存的FP和返回地址）
        List<Parameter> ps = f.parameters();
        for (int i = 0; i < ps.size(); i++) {
            long offset = 16 + i * 8L; // 正数偏移
            paramOffsets.put(ps.get(i), offset);
        }

        // frameSize只需要局部变量的大小，参数在调用者栈帧中
        frameSize = localSize;
        frameSize = (frameSize + 15) & ~15;
    }

    private void genPrologue() {
        assembly.add(new Directive("\tstp\tx29, x30, [sp, #-16]!"));
        assembly.add(new Directive("\tmov\tx29, sp"));
        if (frameSize > 0)
            assembly.add(new Directive("\tsub\tsp, sp, #" + frameSize));

        // 参数已经在调用者栈帧中，不需要额外保存
        // 参数可以通过 [x29, #+16], [x29, #+24] 等直接访问
    }

    private void genEpilogue() {
        if (frameSize > 0)
            assembly.add(new Directive("\tadd\tsp, sp, #" + frameSize));
        assembly.add(new Directive("\tldp\tx29, x30, [sp], #16"));
        assembly.add(new Directive("\tret"));
    }

    /* ====== IR Visitor ====== */

    @Override
    public Void visit(Call e) {
        List<Expr> args = e.args();
        boolean isVar = e.isStaticCall() &&
                e.function().type().getFunctionType().isVararg();

        int total = args.size();
        int fixed = 0;
        if (e.isStaticCall()) {
            fixed = e.function().type().getFunctionType().paramTypes().size();
        }

        int stackArgs = Math.max(0, total - ARG_REGS.length);
        int shadow = isVar ? 64 : 0;
        int temp = total * 8;
        int block = shadow + stackArgs * 8 + temp;
        block = (block + 15) & ~15;

        if (block > 0)
            assembly.add(new Directive("\tsub\tsp, sp, #" + block));

        int tempBase = shadow + stackArgs * 8;

        // Pass1: R->L
        for (int i = total - 1; i >= 0; --i) {
            args.get(i).accept(this); // x0
            assembly.add(new Directive("\tstr\tx0, [sp, #" + (tempBase + i * 8L) + "]"));
        }

        // Pass2: L->R
        for (int i = 0; i < total; ++i) {
            long src = tempBase + i * 8L;
            if (i == 0) {
                assembly.add(new Directive("\tldr\tx0, [sp, #" + src + "]"));
            } else {
                assembly.add(new Directive("\tldr\t" + CALL_TMP + ", [sp, #" + src + "]"));
            }

            String dst;
            if (i < ARG_REGS.length) {
                dst = ARG_REGS[i].toString();
                if (i != 0) {
                    assembly.add(new Directive("\tmov\t" + dst + ", " + CALL_TMP));
                }
            } else {
                dst = (i == 0) ? "x0" : CALL_TMP.toString();
            }

            if (isVar && i >= fixed) {
                long sh = (i - fixed) * 8L;
                assembly.add(new Directive("\tstr\t" + dst + ", [sp, #" + sh + "]"));
            }
            if (i >= ARG_REGS.length) {
                long off = shadow + (long) (i - ARG_REGS.length) * 8;
                assembly.add(new Directive("\tstr\t" + dst + ", [sp, #" + off + "]"));
            }
        }

        if (e.isStaticCall()) {
            assembly.add(new Directive("\tbl\t_" + e.function().name()));
        } else {
            e.expr().accept(this); // callee -> x0
            assembly.add(new Directive("\tblr\tx0"));
        }

        if (block > 0)
            assembly.add(new Directive("\tadd\tsp, sp, #" + block));

        return null;
    }

    @Override
    public Void visit(Assign s) {
        // 参考x86的实现，使用虚拟栈避免寄存器冲突
        if (s.lhs() instanceof Var) {
            // 对于Var类型的LHS，直接计算RHS并存储
            s.rhs().accept(this);
            assembly.add(new Directive("\tmov\t" + VAL_TMP + ", x0"));
            storeToVar((Var) s.lhs(), VAL_TMP);
            return null;
        }

        // 1) 先算 RHS，值在 x0
        s.rhs().accept(this);
        assembly.add(new Directive("\tmov\t" + VAL_TMP + ", x0")); // 保存值

        long sz = s.lhs().type().size(); // 1/2/4/8
        // 生成地址到 x11
        if (s.lhs() instanceof Addr) {
            addrOfEntityInto(((Addr) s.lhs()).entity(), "x11");
        } else if (s.lhs() instanceof Mem) {
            evalAddressInto(((Mem) s.lhs()).expr(), "x11");
        } else {
            errorHandler.error("unsupported LHS in Assign");
            return null;
        }

        // 根据大小选择指令/寄存器宽度
        String xsrc = VAL_TMP.toString(); // x9
        String wsrc = "w" + VAL_TMP.name().substring(1); // w9

        if (sz == 8) {
            assembly.add(new Directive("\tstr\t" + xsrc + ", [x11]"));
        } else if (sz == 4) {
            assembly.add(new Directive("\tstr\t" + wsrc + ", [x11]"));
        } else if (sz == 2) {
            assembly.add(new Directive("\tstrh\t" + wsrc + ", [x11]"));
        } else if (sz == 1) {
            assembly.add(new Directive("\tstrb\t" + wsrc + ", [x11]"));
        } else {
            errorHandler.error("unsupported store size: " + sz);
        }
        return null;
    }

    @Override
    public Void visit(ExprStmt s) {
        s.expr().accept(this);
        return null;
    }

    @Override
    public Void visit(CJump s) {
        s.cond().accept(this);
        assembly.add(new Directive("\tcmp\tx0, #0"));
        assembly.add(new Directive("\tb.ne\t" + s.thenLabel().symbol().toSource(labelSymbols) + "f"));
        assembly.add(new Directive("\tb\t" + s.elseLabel().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    @Override
    public Void visit(Jump s) {
        assembly.add(new Directive("\tb\t" + s.label().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    @Override
    public Void visit(Switch s) {
        s.cond().accept(this);
        assembly.add(new Directive("\tb\t" + s.defaultLabel().symbol()));
        return null;
    }

    @Override
    public Void visit(LabelStmt s) {
        assembly.add(new Directive(s.label().symbol().toSource(labelSymbols) + ":"));
        return null;
    }

    @Override
    public Void visit(Return s) {
        if (s.expr() != null) {
            s.expr().accept(this);
        } else {
            assembly.add(new Directive("\tmov\tx0, #0"));
        }
        assembly.add(new Directive("\tb\t.L" + currentFunction.name() + "_epilogue"));
        return null;
    }

    @Override
    public Void visit(Uni e) {
        e.expr().accept(this);
        switch (e.op()) {
            case UMINUS:
                assembly.add(new Directive("\tneg\tx0, x0"));
                break;
            case BIT_NOT:
                if (e.type().size() == 4) {
                    assembly.add(new Directive("\tmvn\tw0, w0"));
                    assembly.add(new Directive("\tsxtw\tx0, w0"));
                } else {
                    assembly.add(new Directive("\tmvn\tx0, x0"));
                }
                break;
            case NOT:
                assembly.add(new Directive("\tcmp\tx0, #0"));
                assembly.add(new Directive("\tcset\tx0, eq"));
                break;
            case S_CAST:
                // nothing, IR already sign-extended where needed
                break;
            case U_CAST:
                if (e.expr().type().size() == 4 && e.type().size() == 8) {
                    assembly.add(new Directive("\tuxtw\tx0, w0"));
                }
                break;
        }
        return null;
    }

    @Override
    public Void visit(Bin e) {
        // 参考x86的实现，使用栈来避免寄存器冲突
        e.right().accept(this);
        assembly.add(new Directive("\tstr\tx0, [sp, #-16]!")); // 将右操作数压栈
        e.left().accept(this);
        assembly.add(new Directive("\tldr\t" + TMP0 + ", [sp], #16")); // 将右操作数出栈到TMP0

        switch (e.op()) {
            case ADD:
                assembly.add(new Directive("\tadd\tx0, x0, " + TMP0));
                break;
            case SUB:
                assembly.add(new Directive("\tsub\tx0, x0, " + TMP0));
                break;
            case MUL:
                assembly.add(new Directive("\tmul\tx0, x0, " + TMP0));
                break;
            case S_DIV:
                assembly.add(new Directive("\tsdiv\tx0, x0, " + TMP0));
                break;
            case S_MOD:
                assembly.add(new Directive("\tsdiv\t" + ADR_TMP0 + ", x0, " + TMP0));
                assembly.add(new Directive("\tmul\t" + ADR_TMP0 + ", " + ADR_TMP0 + ", " + TMP0));
                assembly.add(new Directive("\tsub\tx0, x0, " + ADR_TMP0));
                break;
            case BIT_AND:
                assembly.add(new Directive("\tand\tx0, x0, " + TMP0));
                break;
            case BIT_OR:
                assembly.add(new Directive("\torr\tx0, x0, " + TMP0));
                break;
            case BIT_XOR:
                assembly.add(new Directive("\teor\tx0, x0, " + TMP0));
                break;
            case BIT_LSHIFT:
                assembly.add(new Directive("\tlsl\tx0, x0, " + TMP0));
                break;
            case ARITH_RSHIFT:
                assembly.add(new Directive("\tasr\tx0, x0, " + TMP0));
                break;
            case EQ:
                assembly.add(new Directive("\tcmp\tx0, " + TMP0));
                assembly.add(new Directive("\tcset\tx0, eq"));
                break;
            case NEQ:
                assembly.add(new Directive("\tcmp\tx0, " + TMP0));
                assembly.add(new Directive("\tcset\tx0, ne"));
                break;
            case S_LT:
                assembly.add(new Directive("\tcmp\tx0, " + TMP0));
                assembly.add(new Directive("\tcset\tx0, lt"));
                break;
            case S_LTEQ:
                assembly.add(new Directive("\tcmp\tx0, " + TMP0));
                assembly.add(new Directive("\tcset\tx0, le"));
                break;
            case S_GT:
                assembly.add(new Directive("\tcmp\tx0, " + TMP0));
                assembly.add(new Directive("\tcset\tx0, gt"));
                break;
            case S_GTEQ:
                assembly.add(new Directive("\tcmp\tx0, " + TMP0));
                assembly.add(new Directive("\tcset\tx0, ge"));
                break;
            default:
                // 保持x0不变
                break;
        }
        return null;
    }

    @Override
    public Void visit(Addr e) {
        addrOfEntity(e.entity());
        return null;
    }

    @Override
    public Void visit(Mem e) {
        // 计算地址到 x11，避免覆盖 x0 里的中间值
        evalAddressInto(e.expr(), "x11");
        long sz = e.type().size();

        if (sz == 1) {
            // 读 1 字节并做有符号扩展成 64 位
            assembly.add(new Directive("\tldrb\tw0, [x11]"));
            assembly.add(new Directive("\tsxtb\tx0, w0"));
        } else if (sz == 2) {
            assembly.add(new Directive("\tldrh\tw0, [x11]"));
            assembly.add(new Directive("\tsxth\tx0, w0"));
        } else if (sz == 4) {
            assembly.add(new Directive("\tldr\tw0, [x11]"));
            assembly.add(new Directive("\tsxtw\tx0, w0"));
        } else if (sz == 8) {
            assembly.add(new Directive("\tldr\tx0, [x11]"));
        } else {
            errorHandler.error("unsupported load size: " + sz);
        }
        return null;
    }

    @Override
    public Void visit(Var e) {
        loadFromVar(e);
        return null;
    }

    @Override
    public Void visit(Int e) {
        long v = e.value();
        if (e.type().size() == 4) {
            assembly.add(new Directive("\tmov\tw0, #" + (v & 0xFFFFFFFFL)));
            assembly.add(new Directive("\tsxtw\tx0, w0"));
        } else {
            materializeImmediate64("x0", v, false);
        }
        return null;
    }

    @Override
    public Void visit(Str e) {
        assembly.add(new Directive("\tadrp\tx0, " + e.entry().symbol() + "@PAGE"));
        assembly.add(new Directive("\tadd\tx0, x0, " + e.entry().symbol() + "@PAGEOFF"));
        return null;
    }

    /* ====== Helpers ====== */

    public AssemblyCode assembly() {
        return assembly;
    }

    private MemoryReference mem(Symbol s) {
        return new DirectMemoryReference(s);
    }

    private ImmediateValue imm(Symbol s) {
        return new ImmediateValue(s);
    }

    private void addrOfEntity(Entity ent) {
        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (off >= 0) {
                assembly.add(new Directive("\tadd\tx0, x29, #" + off));
            } else {
                assembly.add(new Directive("\tsub\tx0, x29, #" + (-off)));
            }
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            assembly.add(new Directive("\tadd\tx0, x29, #" + off));
        } else {
            assembly.add(new Directive("\tadrp\tx0, " + ent.name() + "@PAGE"));
            assembly.add(new Directive("\tadd\tx0, x0, " + ent.name() + "@PAGEOFF"));
        }
    }

    private static final Register ADDR = Register.X11;
    private static final Register OFFT = Register.X12;

    private void evalAddress(Expr e) {
        evalAddressInto(e, ADDR.toString());
    }

    private void evalAddressInto(Expr e, String dst) {
        if (e instanceof Addr) {
            addrOfEntityInto(((Addr) e).entity(), dst);
            return;
        }
        if (e instanceof Var) {
            // 对于Var，我们需要读取变量的值，而不是计算变量的地址
            // 因为Var可能存储的是一个地址（比如@tmp0存储&a[0]）
            loadFromVar((Var) e);
            if (!"x0".equals(dst))
                assembly.add(new Directive("\tmov\t" + dst + ", x0"));
            return;
        }
        if (e instanceof Mem) {
            evalAddressInto(((Mem) e).expr(), dst);
            return;
        }
        if (e instanceof Int) {
            materializeImmediate64(dst, ((Int) e).value(), false);
            return;
        }
        if (e instanceof Bin) {
            Bin b = (Bin) e;
            Op op = b.op();
            if (op == Op.ADD || op == Op.SUB) {
                evalAddressInto(b.left(), dst); // base -> dst
                // 使用临时寄存器计算右边的表达式，避免寄存器冲突
                b.right().accept(this);
                assembly.add(new Directive("\tmov\t" + OFFT + ", x0")); // 将右操作数保存到OFFT
                if (op == Op.ADD)
                    assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + OFFT));
                else
                    assembly.add(new Directive("\tsub\t" + dst + ", " + dst + ", " + OFFT));
                return;
            }
            // 对于其他操作，使用栈操作避免寄存器冲突
            b.right().accept(this);
            assembly.add(new Directive("\tstr\tx0, [sp, #-16]!")); // 将右操作数压栈
            b.left().accept(this);
            assembly.add(new Directive("\tldr\t" + OFFT + ", [sp], #16")); // 将右操作数出栈到OFFT

            switch (op) {
                case MUL:
                    assembly.add(new Directive("\tmul\t" + dst + ", x0, " + OFFT));
                    break;
                case S_DIV:
                    assembly.add(new Directive("\tsdiv\t" + dst + ", x0, " + OFFT));
                    break;
                case S_MOD:
                    assembly.add(new Directive("\tsdiv\t" + TMP0 + ", x0, " + OFFT));
                    assembly.add(new Directive("\tmul\t" + TMP0 + ", " + TMP0 + ", " + OFFT));
                    assembly.add(new Directive("\tsub\t" + dst + ", x0, " + TMP0));
                    break;
                case BIT_AND:
                    assembly.add(new Directive("\tand\t" + dst + ", x0, " + OFFT));
                    break;
                case BIT_OR:
                    assembly.add(new Directive("\torr\t" + dst + ", x0, " + OFFT));
                    break;
                case BIT_XOR:
                    assembly.add(new Directive("\teor\t" + dst + ", x0, " + OFFT));
                    break;
                case BIT_LSHIFT:
                    assembly.add(new Directive("\tlsl\t" + dst + ", x0, " + OFFT));
                    break;
                case ARITH_RSHIFT:
                    assembly.add(new Directive("\tasr\t" + dst + ", x0, " + OFFT));
                    break;
                default:
                    // 对于比较操作，我们不应该在这里处理地址计算
                    errorHandler.error("unsupported binary operation in address calculation: " + op);
                    break;
            }
            return;
        }
        // fallback
        e.accept(this); // x0 assumed = address
        if (!"x0".equals(dst))
            assembly.add(new Directive("\tmov\t" + dst + ", x0"));
    }

    /**
     * addrOfEntity 写到指定寄存器
     */
    private void addrOfEntityInto(Entity ent, String dst) {
        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (off >= 0) {
                assembly.add(new Directive("\tadd\t" + dst + ", x29, #" + off));
            } else {
                assembly.add(new Directive("\tsub\t" + dst + ", x29, #" + (-off)));
            }
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            assembly.add(new Directive("\tadd\t" + dst + ", x29, #" + off));
        } else {
            assembly.add(new Directive("\tadrp\t" + dst + ", " + ent.name() + "@PAGE"));
            assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + ent.name() + "@PAGEOFF"));
        }
    }

    private void loadFromVar(Var v) {
        Entity ent = v.entity();
        long sz = v.type().size();
        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (sz == 8) {
                if (off >= 0) {
                    assembly.add(new Directive("\tldr\tx0, [x29, #" + off + "]"));
                } else {
                    assembly.add(new Directive("\tldr\tx0, [x29, #-" + (-off) + "]"));
                }
            } else {
                if (off >= 0) {
                    assembly.add(new Directive("\tldr\tw0, [x29, #" + off + "]"));
                } else {
                    assembly.add(new Directive("\tldr\tw0, [x29, #-" + (-off) + "]"));
                }
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            }
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            if (sz == 8) {
                assembly.add(new Directive("\tldr\tx0, [x29, #" + off + "]"));
            } else {
                assembly.add(new Directive("\tldr\tw0, [x29, #" + off + "]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            }
        } else {
            assembly.add(new Directive("\tadrp\tx0, " + ent.name() + "@PAGE"));
            assembly.add(new Directive("\tadd\tx0, x0, " + ent.name() + "@PAGEOFF"));
            if (sz == 8) {
                assembly.add(new Directive("\tldr\tx0, [x0]"));
            } else {
                assembly.add(new Directive("\tldr\tw0, [x0]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            }
        }
    }

    private void storeToVar(Var v, Register src) {
        Entity ent = v.entity();
        long sz = v.type().size();
        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (sz == 8) {
                if (off >= 0) {
                    assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + off + "]"));
                } else {
                    assembly.add(new Directive("\tstr\t" + src + ", [x29, #-" + (-off) + "]"));
                }
            } else {
                String w = "w" + src.name().substring(1);
                if (off >= 0) {
                    assembly.add(new Directive("\tstr\t" + w + ", [x29, #" + off + "]"));
                } else {
                    assembly.add(new Directive("\tstr\t" + w + ", [x29, #-" + (-off) + "]"));
                }
            }
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + off + "]"));
        } else {
            assembly.add(new Directive("\tadrp\tx0, " + ent.name() + "@PAGE"));
            assembly.add(new Directive("\tadd\tx0, x0, " + ent.name() + "@PAGEOFF"));
            assembly.add(new Directive("\tstr\t" + src + ", [x0]"));
        }
    }

    /**
     * mov/movk/movn build 64-bit imm; signExtend32 为 true 时按 32 位有符号扩展
     */
    /**
     * 判断一个表达式是否是地址计算表达式
     */
    private boolean isAddressExpression(Expr e) {
        if (e instanceof Addr) {
            return true;
        }
        if (e instanceof Bin) {
            Bin b = (Bin) e;
            return b.op() == Op.ADD || b.op() == Op.SUB;
        }
        return false;
    }

    private void materializeImmediate64(String reg, long v, boolean signExtend32) {
        if (signExtend32) {
            long w = v & 0xFFFFFFFFL;
            assembly.add(new Directive("\tmov\tw0, #" + w));
            assembly.add(new Directive("\tsxtw\tx0, w0"));
            if (!"x0".equals(reg)) {
                assembly.add(new Directive("\tmov\t" + reg + ", x0"));
            }
            return;
        }
        if (v < 0) {
            long uv = ~(-v) & 0xFFFFFFFFFFFFFFFFL;
            assembly.add(new Directive("\tmovn\t" + reg + ", #" + (uv & 0xFFFF)));
            for (int s = 16; s < 64; s += 16) {
                long chunk = (uv >>> s) & 0xFFFF;
                if (chunk != 0)
                    assembly.add(new Directive("\tmovk\t" + reg + ", #" + chunk + ", lsl #" + s));
            }
        } else {
            assembly.add(new Directive("\tmov\t" + reg + ", #" + (v & 0xFFFF)));
            for (int s = 16; s < 64; s += 16) {
                long chunk = (v >>> s) & 0xFFFF;
                if (chunk != 0)
                    assembly.add(new Directive("\tmovk\t" + reg + ", #" + chunk + ", lsl #" + s));
            }
        }
    }

    /**
     * 只声明，不在这里给实现，防止你又被我改错
     */
    private String escapeString(String s) {
        // use your own correct version
        return s.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n");
    }

    private void storeSized(String addrReg, String srcReg, long size, boolean isUnsigned) {
        if (size == 1) {
            String wsrc = srcReg.replace('x', 'w');
            assembly.add(new Directive("\tstrb\t" + wsrc + ", [" + addrReg + "]"));
        } else if (size == 4) {
            String wsrc = srcReg.replace('x', 'w');
            assembly.add(new Directive("\tstr\t" + wsrc + ", [" + addrReg + "]"));
        } else {
            assembly.add(new Directive("\tstr\t" + srcReg + ", [" + addrReg + "]"));
        }
    }

    private void loadSized(String addrReg, String dstReg, long size, boolean signExtend) {
        if (size == 1) {
            assembly.add(new Directive("\tldrb\tw0, [" + addrReg + "]"));
            if (signExtend)
                assembly.add(new Directive("\tsxtb\t" + dstReg + ", w0"));
            else
                assembly.add(new Directive("\tuxtb\t" + dstReg + ", w0"));
        } else if (size == 4) {
            assembly.add(new Directive("\tldr\tw0, [" + addrReg + "]"));
            if (signExtend)
                assembly.add(new Directive("\tsxtw\t" + dstReg + ", w0"));
            else
                assembly.add(new Directive("\tuxtw\t" + dstReg + ", w0"));
        } else {
            assembly.add(new Directive("\tldr\t" + dstReg + ", [" + addrReg + "]"));
        }
    }

}
