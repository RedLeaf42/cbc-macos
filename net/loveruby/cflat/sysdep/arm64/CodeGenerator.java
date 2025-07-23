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
    private static final Register[] ARG_REGS = {Register.X0, Register.X1, Register.X2, Register.X3,
            Register.X4, Register.X5, Register.X6, Register.X7};
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

    private Set<Register> usedRegisters;

    public CodeGenerator(net.loveruby.cflat.sysdep.CodeGeneratorOptions opts,
                         net.loveruby.cflat.asm.Type naturalType, ErrorHandler h) {
        this.options = opts;
        this.naturalType = naturalType;
        this.errorHandler = h;
        this.assembly = new AssemblyCode(h);
        this.constSymbols = new SymbolTable(".LC");
        this.labelSymbols = new SymbolTable("");
        this.localVarOffsets = new HashMap<>();
        this.paramOffsets = new HashMap<>();
        this.usedRegisters = new HashSet<>();
    }

    public AssemblyCode generate(net.loveruby.cflat.ir.IR ir) {
        generateDataSection(ir);
        generateTextSection(ir);
        return assembly;
    }

    /** return log2(align), minimum 'minExp' */
    private int pow2AlignExp(long size, int minExp) {
        // nextPow2
        long n = 1;
        while (n < size) n <<= 1;
        int e = 0;
        while ((1L << e) < n) e++;
        return Math.max(e, minExp);
    }

    private void generateDataSection(net.loveruby.cflat.ir.IR ir) {
        // ---------- 1. string literals ----------
        for (ConstantEntry ent : ir.constantTable().entries()) {
            Symbol sym = constSymbols.newSymbol();  // .LCx
            ent.setSymbol(sym);
            ent.setMemref(mem(sym));
            ent.setAddress(imm(sym));

            assembly.add(new Directive("\t.section\t__TEXT,__cstring,cstring_literals"));
            assembly.add(new Label(sym));
            assembly.add(new Directive("\t.asciz\t\"" + escapeString(ent.value()) + "\""));
        }

        // ---------- 2. initialized globals ----------
        for (DefinedVariable var : ir.definedGlobalVariables()) {
            Symbol sym = new NamedSymbol(var.name());
            var.setMemref(mem(sym));
            var.setAddress(imm(sym));

            assembly.add(new Directive("\t.section\t__DATA,__data"));
            // external global?
            assembly.add(new Directive("\t.globl\t_" + var.name()));
            assembly.add(new Label(sym));

            if (var.hasInitializer()) {
                Object init = var.initializer(); // depends on your IR/AST types

                if (init instanceof net.loveruby.cflat.ast.IntegerLiteralNode) {
                    long v = ((net.loveruby.cflat.ast.IntegerLiteralNode) init).value();
                    assembly.add(new Directive("\t.quad\t" + v));
                }
                else if (init instanceof net.loveruby.cflat.ast.StringLiteralNode) {
                    // find the ConstantEntry again (or create if needed)
                    String s = ((net.loveruby.cflat.ast.StringLiteralNode) init).value();
                    ConstantEntry ce = ir.constantTable().intern(s);
                    Symbol cs = ce.symbol();
                    assembly.add(new Directive("\t.quad\t" + cs.toSource()));
                }
                else if (init instanceof net.loveruby.cflat.ir.Str) {
                    Symbol cs = ((net.loveruby.cflat.ir.Str) init).entry().symbol();
                    assembly.add(new Directive("\t.quad\t" + cs.toSource()));
                }
                else {
                    // TODO: handle arrays/structs/etc. if your IR supports
                    assembly.add(new Directive("\t.quad\t0"));
                }
            }
            else {
                // explicitly zero-initialized global with no initializer
                assembly.add(new Directive("\t.space\t" + var.type().size()));
            }
        }

        // ---------- 3. common symbols (uninitialized extern) ----------
        for (DefinedVariable var : ir.definedCommonSymbols()) {
            long size  = var.type().size();
            int  align = pow2AlignExp(size, 3); // at least 8-byte align on AArch64

            Symbol sym = new NamedSymbol(var.name());
            var.setMemref(mem(sym));
            var.setAddress(imm(sym));

            // external by default
            assembly.add(new Directive("\t.globl\t_" + var.name()));
            // Mach-O way: zerofill in __common
            assembly.add(new Directive("\t.zerofill\t__DATA,__common,_"
                    + var.name() + "," + size + "," + align));
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
        localVarOffsets.clear();
        paramOffsets.clear();
        usedRegisters.clear();
        calculateStackLayout(func);

        Symbol funcSym = new NamedSymbol("_" + func.name());
        func.setCallingSymbol(funcSym);
        func.setMemref(mem(funcSym));
        func.setAddress(imm(funcSym));

        assembly.add(new Directive("\t.globl\t_" + func.name()));
        assembly.add(new Directive("\t.p2align\t2"));
        assembly.add(new Label(funcSym));

        generatePrologue(func);
        for (Stmt stmt : func.ir()) {
            stmt.accept(this);
        }
        assembly.add(new Label(new NamedSymbol(".L" + func.name() + "_epilogue")));
        generateEpilogue(func);
    }

    private void calculateStackLayout(DefinedFunction func) {
        List<DefinedVariable> allLocals = func.lvarScope().allVariablesWithPrivate();
        long localSize = 0;
        for (DefinedVariable v : allLocals) {
            if (v.isParameter()) continue;
            long size = (v.type().allocSize() + 7) & ~7;
            localVarOffsets.put(v, localSize);
            localSize += size;
        }

        List<Parameter> params = func.parameters();
        long paramSize = params.size() * 8;
        for (int i = 0; i < params.size(); i++) {
            paramOffsets.put(params.get(i), localSize + i * 8);
        }

        stackSize = localSize + paramSize + 16;
        stackSize = (stackSize + 15) & ~15;
    }

    private void generatePrologue(DefinedFunction func) {
        assembly.add(new Directive("\tsub\tsp, sp, #" + stackSize));
        assembly.add(new Directive("\tstp\tx29, x30, [sp, #" + (stackSize - 16) + "]"));
        assembly.add(new Directive("\tmov\tx29, sp"));

        // 修复：定义params变量
        List<Parameter> params = func.parameters();
        for (int i = 0; i < params.size() && i < ARG_REGS.length; i++) {
            long offset = paramOffsets.get(params.get(i));
            assembly.add(new Directive("\tstr\t" + ARG_REGS[i] + ", [x29, #" + offset + "]"));
        }
    }

//    private void generateEpilogue(DefinedFunction func) {
//        assembly.add(new Directive("\tldp\tx29, x30, [sp, #" + (stackSize - 16) + "]"));
//        assembly.add(new Directive("\tadd\tsp, sp, #" + stackSize));
//        assembly.add(new Directive("\tret"));
//    }

    @Override
    public Void visit(ExprStmt stmt) {
        stmt.expr().accept(this);
        return null;
    }

    @Override
    public Void visit(Assign stmt) {
        stmt.rhs().accept(this);
        assembly.add(new Directive("\tmov\tx1, x0"));

        if (stmt.lhs().isVar()) {
            Var var = (Var) stmt.lhs();
            Entity entity = var.entity();

            if (localVarOffsets.containsKey(entity)) {
                long offset = localVarOffsets.get(entity);
                if (var.type().size() == 8) {
                    assembly.add(new Directive("\tstr\tx1, [x29, #" + offset + "]"));
                } else {
                    assembly.add(new Directive("\tstr\tw1, [x29, #" + offset + "]"));
                }
            } else if (paramOffsets.containsKey(entity)) {
                long offset = paramOffsets.get(entity);
                assembly.add(new Directive("\tstr\tx1, [x29, #" + offset + "]"));
            } else {
                assembly.add(new Directive("\tadrp\tx0, " + entity.name() + "@PAGE"));
                assembly.add(new Directive("\tadd\tx0, x0, " + entity.name() + "@PAGEOFF"));
                assembly.add(new Directive("\tstr\tx1, [x0]"));
            }
        } else if (stmt.lhs().isAddr()) {
            Addr addr = (Addr) stmt.lhs();
            Entity entity = addr.entity();
            if (localVarOffsets.containsKey(entity)) {
                long offset = localVarOffsets.get(entity);
                assembly.add(new Directive("\tstr\tx1, [x29, #" + offset + "]"));
            } else if (paramOffsets.containsKey(entity)) {
                long offset = paramOffsets.get(entity);
                assembly.add(new Directive("\tstr\tx1, [x29, #" + offset + "]"));
            } else {
                assembly.add(new Directive("\tadrp\tx0, " + entity.name() + "@PAGE"));
                assembly.add(new Directive("\tadd\tx0, x0, " + entity.name() + "@PAGEOFF"));
                assembly.add(new Directive("\tstr\tx1, [x0]"));
            }
        } else {
            if (stmt.lhs() instanceof Mem) {
                Mem mem = (Mem) stmt.lhs();
                if (mem.expr() instanceof Mem) {
                    Mem innerMem = (Mem) mem.expr();
                    innerMem.expr().accept(this);
                    assembly.add(new Directive("\tmov\tx2, x0"));
                    assembly.add(new Directive("\tldr\tx0, [x2]"));
                } else {
                    mem.expr().accept(this);
                }
            } else {
                stmt.lhs().accept(this);
            }
            assembly.add(new Directive("\tstr\tx1, [x0]"));
        }

        return null;
    }

    @Override
    public Void visit(CJump stmt) {
        stmt.cond().accept(this);
        assembly.add(new Directive("\tcmp\tx0, #0"));
        assembly.add(new Directive("\tb.ne\t" + stmt.thenLabel().symbol().toSource(labelSymbols) + "f"));
        assembly.add(new Directive("\tb\t" + stmt.elseLabel().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    @Override
    public Void visit(Jump stmt) {
        assembly.add(new Directive("\tb\t" + stmt.label().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    @Override
    public Void visit(Switch stmt) {
        stmt.cond().accept(this);
        assembly.add(new Directive("\tb\t" + stmt.defaultLabel().symbol()));
        return null;
    }

    @Override
    public Void visit(LabelStmt stmt) {
        String label = stmt.label().symbol().toSource(labelSymbols);
        assembly.add(new Directive(label + ":"));
        return null;
    }

    @Override
    public Void visit(Return stmt) {
        if (stmt.expr() != null) {
            stmt.expr().accept(this);
        } else {
            assembly.add(new Directive("\tmov\tx0, #0"));
        }
        assembly.add(new Directive("\tb\t.L" + currentFunction.name() + "_epilogue"));
        return null;
    }

    @Override
    public Void visit(Uni expr) {
        expr.expr().accept(this);

        switch (expr.op()) {
            case UMINUS:
                assembly.add(new Directive("\tneg\tx0, x0"));
                break;
            case BIT_NOT:
                if (expr.type().size() == 4) {
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
                if (expr.expr().type().size() == 4 && expr.type().size() == 8) {
                    // 保持空实现，依赖后续扩展
                }
                break;
            case U_CAST:
                if (expr.expr().type().size() == 4 && expr.type().size() == 8) {
                    assembly.add(new Directive("\tuxtw\tx0, w0"));
                }
                break;
        }
        return null;
    }

    @Override
    public Void visit(Bin expr) {
        expr.left().accept(this);
        assembly.add(new Directive("\tmov\tx1, x0"));
        expr.right().accept(this);
        assembly.add(new Directive("\tmov\tx2, x0"));

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
                assembly.add(new Directive("\tmov\tx0, x1"));
                break;
        }
        return null;
    }

    /**
     * CodeGenerator.java (ARM64 for macOS) — Variadic call final fix
     * <p>
     * Strategy:
     * 1. Evaluate ALL arguments to a temporary area on stack (right-to-left) so later evaluations
     * cannot clobber earlier values.
     * 2. After all args are materialized, load them back into the proper ABI registers (x0–x7) and
     * copy every register arg into the 64-byte shadow space. Extra args (>8) go after shadow.
     * 3. For non-variadic calls we still only spill args >8.
     * <p>
     * This avoids:
     * - Overwriting x0 (format string) when evaluating later args.
     * - Re-using ARG_REGS that our IR evaluation code also uses as scratch (x1/x2/x3...).
     */
    @Override
    public Void visit(Call expr) {
        final List<Expr> args = expr.args();
        final boolean isVariadic = expr.isStaticCall() &&
                expr.function().type().getFunctionType().isVararg();

        /*------------------------------------------------------------
         *  Stack layout planning
         *----------------------------------------------------------*/
        int totalArgs = args.size();
        int fixedArgCount = 0;
        if (expr.isStaticCall()) {
            fixedArgCount = expr.function().type().getFunctionType().paramTypes().size();
        }

        int shadowSize = isVariadic ? 64 : 0;                  // Apple ABI shadow
        int extraArgs = Math.max(0, totalArgs - ARG_REGS.length);
        int tempSize = totalArgs * 8;                        // temp spill for all args

        int totalStack = shadowSize + extraArgs * 8 + tempSize;
        totalStack = (totalStack + 15) & ~15;              // 16‑byte align

        if (totalStack > 0)
            assembly.add(new Directive("	sub	sp, sp, #" + totalStack));

        final int tempBase = shadowSize + extraArgs * 8;       // temp region offset
        final String SCR = "x9";                             // dedicated scratch to avoid clobbering x0

        /*------------------------------------------------------------
         *  Pass 1 – evaluate right‑to‑left, spill to temp
         *----------------------------------------------------------*/
        for (int i = totalArgs - 1; i >= 0; --i) {
            args.get(i).accept(this);             // result -> x0 (safe, we'll spill immediately)
            assembly.add(new Directive("	str	x0, [sp, #" + (tempBase + i * 8L) + "]"));
        }

        /*------------------------------------------------------------
         *  Pass 2 – load temp, assign regs, build shadow/stack
         *----------------------------------------------------------*/
        for (int i = 0; i < totalArgs; ++i) {
            long srcOff = tempBase + i * 8L;

            if (i == 0) {
                // first argument ALWAYS to x0 (usually format string)
                assembly.add(new Directive("	ldr	x0, [sp, #" + srcOff + "]"));
            } else {
                assembly.add(new Directive("	ldr	" + SCR + ", [sp, #" + srcOff + "]"));
            }

            String dstReg;
            if (i < ARG_REGS.length) {
                dstReg = ARG_REGS[i].toString();
                if (i == 0) {
                    /* already in x0 */
                } else {
                    assembly.add(new Directive("	mov	" + dstReg + ", " + SCR));
                }
            } else {
                dstReg = (i == 0) ? "x0" : SCR;   // will be pushed to stack below
            }

            /* ---- copy to shadow / stack slots ---- */
            if (isVariadic) {
                // only variadic arguments (>=fixedArgCount) need shadow copy
                if (i >= fixedArgCount) {
                    int varIdx = i - fixedArgCount;
                    long shOff = varIdx * 8L;                     // sequential slots
                    String src = dstReg;
                    assembly.add(new Directive("	str	" + src + ", [sp, #" + shOff + "]"));
                }
            }

            // stack args beyond register window
            if (i >= ARG_REGS.length) {
                long off = shadowSize + (long) (i - ARG_REGS.length) * 8;
                assembly.add(new Directive("	str	" + dstReg + ", [sp, #" + off + "]"));
            }
        }

        /*------------------------------------------------------------
         *  Make the call
         *----------------------------------------------------------*/
        if (expr.isStaticCall()) {
            assembly.add(new Directive("	bl	_" + expr.function().name()));
        } else {
            expr.expr().accept(this);         // target -> x0
            assembly.add(new Directive("	blr	x0"));
        }

        if (totalStack > 0)
            assembly.add(new Directive("	add	sp, sp, #" + totalStack));

        return null;
    }


    @Override
    public Void visit(Addr expr) {
        Entity ent = expr.entity();
        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            assembly.add(new Directive("\tadd\tx0, x29, #" + off));
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            assembly.add(new Directive("\tadd\tx0, x29, #" + off));
        } else {
            assembly.add(new Directive("\tadrp\tx0, " + ent.name() + "@PAGE"));
            assembly.add(new Directive("\tadd\tx0, x0, " + ent.name() + "@PAGEOFF"));
        }
        return null;
    }

    @Override
    public Void visit(Mem expr) {
        expr.expr().accept(this);
        if (expr.type().size() == 1) {
            assembly.add(new Directive("\tldrb\tw0, [x0]"));
            assembly.add(new Directive("\tsxtb\tx0, w0"));
        } else if (expr.type().size() == 4) {
            assembly.add(new Directive("\tldr\tw0, [x0]"));
            assembly.add(new Directive("\tsxtw\tx0, w0"));
        } else {
            assembly.add(new Directive("\tldr\tx0, [x0]"));
        }
        return null;
    }

    @Override
    public Void visit(Var expr) {
        Entity ent = expr.entity();

        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (expr.type().size() == 8) {
                assembly.add(new Directive("\tldr\tx0, [x29, #" + off + "]"));
            } else {
                assembly.add(new Directive("\tldr\tw0, [x29, #" + off + "]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            }
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            if (expr.type().size() == 8) {
                assembly.add(new Directive("\tldr\tx0, [x29, #" + off + "]"));
            } else {
                assembly.add(new Directive("\tldr\tw0, [x29, #" + off + "]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            }
        } else {
            assembly.add(new Directive("\tadrp\tx0, " + ent.name() + "@PAGE"));
            assembly.add(new Directive("\tadd\tx0, x0, " + ent.name() + "@PAGEOFF"));
            assembly.add(new Directive("\tldr\tx0, [x0]"));
        }
        return null;
    }

    @Override
    public Void visit(Int expr) {
        long v = expr.value();

        if (expr.type().size() == 4) {
            assembly.add(new Directive("\tmov\tw0, #" + (v & 0xFFFFFFFFL)));
            assembly.add(new Directive("\tsxtw\tx0, w0"));
        } else {
            if (v < 0) {
                long uv = ~(-v) & 0xFFFFFFFFFFFFFFFFL;
                assembly.add(new Directive("\tmovn\tx0, #" + (uv & 0xFFFF)));
                for (int s = 16; s < 64; s += 16) {
                    if (((uv >> s) & 0xFFFF) != 0) {
                        assembly.add(new Directive("\tmovk\tx0, #" + ((uv >> s) & 0xFFFF) + ", lsl #" + s));
                    }
                }
            } else {
                assembly.add(new Directive("\tmov\tx0, #" + (v & 0xFFFF)));
                for (int s = 16; s < 64; s += 16) {
                    if (((v >> s) & 0xFFFF) != 0) {
                        assembly.add(new Directive("\tmovk\tx0, #" + ((v >> s) & 0xFFFF) + ", lsl #" + s));
                    }
                }
            }
        }
        return null;
    }

    @Override
    public Void visit(Str expr) {
        assembly.add(new Directive("\tadrp\tx0, " + expr.entry().symbol() + "@PAGE"));
        assembly.add(new Directive("\tadd\tx0, x0, " + expr.entry().symbol() + "@PAGEOFF"));
        return null;
    }

    public AssemblyCode assembly() {
        return assembly;
    }

    private MemoryReference mem(Symbol sym) {
        return new DirectMemoryReference(sym);
    }

    private ImmediateValue imm(Symbol sym) {
        return new ImmediateValue(sym);
    }

    private String escapeString(String str) {
        return str.replace("\\", "\\\\").replace("\"", "\\\"").replace("\n", "\\n");
    }

    private void generateEpilogue(DefinedFunction func) {
        assembly.add(new Directive("\tldp\tx29, x30, [sp, #" + (stackSize - 16) + "]"));
        assembly.add(new Directive("\tadd\tsp, sp, #" + stackSize));
        assembly.add(new Directive("\tret"));
    }
}