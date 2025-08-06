package net.loveruby.cflat.sysdep.arm64;

import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.ir.*;
import net.loveruby.cflat.utils.ErrorHandler;

import java.util.*;
import java.util.stream.Collectors;

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

    private final SymbolTable constSymbols = new SymbolTable(".LC");
    private final SymbolTable labelSymbols = new SymbolTable("L");

    // per-function context
    private DefinedFunction currentFunction;
    private IR currentIR; // 当前编译单元的IR
    private long frameSize; // size we sub after pushing FP/LR
    private long spillOffset;
    private final Map<Entity, Long> localVarOffsets = new HashMap<>();
    private final Map<Entity, Long> paramOffsets = new HashMap<>();
    private final List<DefinedVariable> staticLocals = new ArrayList<>();
    // 寄存器分配器
    private RegisterAllocator registerAllocator;
    private Map<Entity, net.loveruby.cflat.asm.Register> registerMap = new HashMap<>();
    private Map<Entity, Long> spillOffsets = new HashMap<>();

    public CodeGenerator(net.loveruby.cflat.sysdep.CodeGeneratorOptions opts,
            net.loveruby.cflat.asm.Type naturalType,
            ErrorHandler h) {
        this.options = opts;
        this.naturalType = naturalType;
        this.errorHandler = h;
        this.assembly = new AssemblyCode(h);
        this.registerAllocator = new RegisterAllocator();
    }

    /* ====== Entry ====== */
    @Override
    public AssemblyCode generate(IR ir) {
        currentIR = ir; // 保存当前IR的引用
        System.err.println("generate: " + ir.allGlobalVariables().stream()
                .map((java.util.function.Function<Variable, Object>) Entity::name).collect(Collectors.toList()));
        collectStaticLocals(ir);
        locateSymbols(ir);
        // 为静态变量设置符号引用
        for (DefinedVariable var : staticLocals) {
            locateGlobalVariable(var);
        }

        generateDataSection(ir);
        generateTextSection(ir);
        return assembly;
    }

    private void locateSymbols(IR ir) {
        // 为所有函数设置符号引用
        for (Function func : ir.allFunctions()) {
            locateFunction(func);
        }

        // 为所有全局变量设置符号引用
        for (DefinedVariable var : ir.definedGlobalVariables()) {
            locateGlobalVariable(var);
        }

        // 为所有common symbols设置符号引用
        for (DefinedVariable var : ir.definedCommonSymbols()) {
            locateGlobalVariable(var);
        }

        // 为所有静态局部变量设置符号引用
        for (DefinedVariable var : staticLocals) {
            locateGlobalVariable(var);
        }

        // 为所有外部函数设置符号引用（这些函数在IR中通过Addr节点引用）
        // 这里我们无法直接获取所有外部函数，但可以在addrOfEntity中动态处理
    }

    private void locateFunction(Function func) {
        func.setCallingSymbol(callingSymbol(func));
        locateGlobalVariable(func);
    }

    private Symbol callingSymbol(Function func) {
        if (func.isPrivate()) {
            return new NamedSymbol(func.name());
        } else {
            Symbol sym = new NamedSymbol("_" + func.name());
            return shouldUsePLT(func) ? PLTSymbol(sym) : sym;
        }
    }

    private boolean shouldUsePLT(Entity ent) {
        if (!options.isPositionIndependent()) {
            return false;
        }

        // 检查函数是否在当前编译单元中定义
        if (ent instanceof Function && currentIR != null) {
            for (DefinedFunction func : currentIR.definedFunctions()) {
                if (func.name().equals(ent.name())) {
                    return false; // 在当前编译单元中定义的函数不使用PLT
                }
            }
        }

        return true; // 外部函数使用PLT
    }

    private Symbol PLTSymbol(Symbol base) {
        return new SuffixedSymbol(base, "");
    }

    private void locateGlobalVariable(Entity ent) {
        Symbol sym;
        if (ent instanceof Function) {
            sym = ((Function) ent).callingSymbol();
        } else {
            // 对于非函数实体，统一使用带下划线前缀的符号名
            String symbolName = "_" + ent.name();
            sym = new NamedSymbol(symbolName);
        }

        if (options.isPositionIndependent()) {
            if (ent.isPrivate() || optimizeGvarAccess(ent)) {
                // 对于私有符号或在当前编译单元中定义的符号，使用@PAGE/@PAGEOFF
                ent.setMemref(mem(sym));
                ent.setAddress(imm(sym));
            } else {
                // 对于外部符号，使用@GOT重定位
                ent.setMemref(mem(globalGOTSymbol(sym)));
                ent.setAddress(imm(globalGOTSymbol(sym)));
            }
        } else {
            // 非PIC模式，使用直接地址
            ent.setMemref(mem(sym));
            ent.setAddress(imm(sym));
        }
    }

    private boolean optimizeGvarAccess(Entity ent) {
        return options.isPIERequired() && ent.isDefined();
    }

    private Symbol globalGOTSymbol(Symbol base) {
        return new SuffixedSymbol(base, "@GOT");
    }

    private void collectStaticLocals(IR ir) {
        staticLocals.clear();
        // 考虑全局变量中定义的静态变量
        for (DefinedVariable global : ir.definedCommonSymbols()) {
            if (global.isPrivate()) {
                staticLocals.add(global);
            }
        }
    }

    /* ====== Data ====== */
    private void generateDataSection(IR ir) {
        // 1. string literals
        for (ConstantEntry ent : ir.constantTable().entries()) {
            Symbol sym = constSymbols.newSymbol();
            ent.setSymbol(sym);
            if (options.isPositionIndependent()) {
                // 在PIC模式下，字符串字面量使用@PAGE/@PAGEOFF重定位
                ent.setMemref(mem(sym));
                ent.setAddress(imm(sym));
            } else {
                ent.setMemref(mem(sym));
                ent.setAddress(imm(sym));
            }
            assembly.add(new Directive("\t.section\t__TEXT,__cstring,cstring_literals"));
            assembly.add(new Label(sym));
            assembly.add(new Directive("\t.asciz\t\"" + escapeString(ent.value()) + "\""));
        }

        // 2. initialized globals (both public and private) 初始化的静态/非静态全局变量
        for (DefinedVariable var : ir.definedGlobalVariables()) {
            emitInitializedGlobal(var, !var.isPrivate(), ir);
        }
        // 3. static locals (private in IR) 没有初始化的静态全局变量
        for (DefinedVariable var : staticLocals) {
            emitInitializedGlobal(var, false, ir);
        }
        // 4. common (tentative) symbols 没有初始化的全局变量,理论上来说不应该包含静态变量的
        for (DefinedVariable var : ir.definedCommonSymbols()) {
            // private变量不发射到这个区域
            if (!var.isPrivate()) {
                emitCommonSymbol(var);
            } else {
                System.err.println("exclude static variable " + var.name());
            }
        }
    }

    private void emitInitializedGlobal(DefinedVariable var, boolean external, IR ir) {
        // 对于全局变量，使用带下划线前缀的符号名
        String symbolName = "_" + var.name();
        Symbol sym = new NamedSymbol(symbolName);
        var.setMemref(mem(sym));
        var.setAddress(imm(sym));

        assembly.add(new Directive("\t.section\t__DATA,__data"));
        if (external) {
            assembly.add(new Directive("\t.globl\t" + symbolName));
        } else {
            assembly.add(new Directive("\t.private_extern\t" + symbolName));
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
            assembly.add(new Directive("\t.space\t" + var.type().allocSize()));
        }
    }

    private void emitCommonSymbol(DefinedVariable var) {
        long size = var.type().size();
        int align = Math.max(3, log2ceil(size)); // >= 8 byte

        // 对于全局变量，使用带下划线前缀的符号名
        String symbolName = "_" + var.name();
        Symbol sym = new NamedSymbol(symbolName);
        var.setMemref(mem(sym));
        var.setAddress(imm(sym));

        assembly.add(new Directive("\t.globl\t" + symbolName));
        assembly.add(new Directive("\t.zerofill\t__DATA,__common," + symbolName + "," + size + "," + align));
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

    /**
     *
     * 栈区设计
     * -----
     * 超过8个参数在栈上
     * -----
     * x29
     * -----
     * x30
     * ----
     * callee寄存器使用保存
     * ----
     * 局部变量区
     * ----
     * 寄存器临时spill区域,当使用临时寄存器不够的时候就spill到这个区域
     *
     */
    private void generateFunction(DefinedFunction func) {
        currentFunction = func;
        localVarOffsets.clear();
        paramOffsets.clear();

        // 获取寄存器分配结果
        registerMap.clear();
        spillOffsets.clear();
        calcFrameLayout(func);
        // 重新进行寄存器分配（确保获取最新结果）
        registerAllocator.allocateRegisters(func, paramOffsets, localVarOffsets, currentIR.allGlobalVariables(),
                func.lvarScope());
        long allocatedRegisterSize = registerAllocator.getAllocatedRegisterOrderedList().size() * 8L;
        System.out.println("frameSize original " + frameSize + " registerSize=" + allocatedRegisterSize);
        frameSize += allocatedRegisterSize;
        /*
         * 调整偏移量
         * 对于params offset只需要调整负数的部分
         */
        final List<Entity> fixOffsetEntityList = new ArrayList<>();
        paramOffsets.forEach((entity, offset) -> {
            if (offset < 0) {
                fixOffsetEntityList.add(entity);
            }
        });

        for (Entity entity : fixOffsetEntityList) {
            long offset = paramOffsets.get(entity);
            paramOffsets.put(entity, offset - allocatedRegisterSize);
        }
        for (Entity entity : localVarOffsets.keySet()) {
            long offset = localVarOffsets.get(entity);
            localVarOffsets.put(entity, offset - allocatedRegisterSize);
        }
        registerAllocator.adjustSpill(paramOffsets, localVarOffsets);
        // 获取分配结果
        for (DefinedVariable var : func.localVariables()) {
            Entity entity = var;
            if (registerAllocator.isInRegister(entity)) {
                System.err.println("InRegister " + entity.name());
                registerMap.put(entity, registerAllocator.getRegister(entity));
            } else if (registerAllocator.isSpilled(entity)) {
                System.err.println("InSpill " + entity.name());
                spillOffsets.put(entity, registerAllocator.getSpillOffset(entity));
            }
        }

        // 处理参数
        for (Parameter param : func.parameters()) {
            if (registerAllocator.isInRegister(param)) {
                registerMap.put(param, registerAllocator.getRegister(param));
            } else if (registerAllocator.isSpilled(param)) {
                spillOffsets.put(param, registerAllocator.getSpillOffset(param));
            }
        }

        // 对于函数定义，我们使用不带@PLT后缀的符号名
        String funcName = func.isPrivate() ? func.name() : "_" + func.name();

        if (func.isPrivate()) {
            assembly.add(new Directive("\t.private_extern\t" + funcName));
        } else {
            assembly.add(new Directive("\t.globl\t" + funcName));
        }
        assembly.add(new Directive("\t.p2align\t2"));
        assembly.add(new Label(new NamedSymbol(funcName)));
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
        // 在ARM64中，前8个参数在寄存器中，额外的参数在栈上
        List<Parameter> ps = f.parameters();
        for (int i = 0; i < ps.size(); i++) {
            if (i < 8) {
                // 前8个参数在寄存器中，使用特殊的偏移值表示
                paramOffsets.put(ps.get(i), -((long) i + 1) * 8);
            } else {
                // 额外的参数在栈上
                long offset = 16 + (i - 8) * 8L; // 正数偏移
                paramOffsets.put(ps.get(i), offset);
            }
        }

        // 计算参数保存空间
        long paramSaveSize = 0;
        for (int i = 0; i < ps.size() && i < 8; i++) {
            paramSaveSize += 8; // 每个参数占8字节
        }

        // 递归分配局部变量空间，允许不同block重叠
        // 从参数保存空间之后开始分配
        LocalScope root = f.lvarScope();

        // 总栈帧大小 = 局部变量大小 + 参数保存空间
        frameSize = allocateLocalVariablesRecursive(root, paramSaveSize);
    }

    // 递归分配局部变量空间，返回当前及所有子作用域最大所需空间
    private long allocateLocalVariablesRecursive(LocalScope scope, long parentStackLen) {
        long len = parentStackLen;
        // 当前作用域变量顺序分配
        for (DefinedVariable var : scope.localVariables()) {
            if (var.isParameter() || var.isPrivate())
                continue;
            long alignment = var.type().alignment();
            len = (len + alignment - 1) & -alignment;
            long sz = var.type().allocSize();
            len += sz;
            // 先设置为临时偏移，后面fix
            localVarOffsets.put(var, -len);
        }
        // 递归分配子作用域，空间可重叠
        long maxLen = len;
        for (LocalScope s : scope.children()) {
            long childLen = allocateLocalVariablesRecursive(s, len);
            maxLen = Math.max(maxLen, childLen);
        }
        return maxLen;
    }

    private void genPrologue() {
        assembly.add(new Directive("\tstp\tx29, x30, [sp, #-16]!"));
        assembly.add(new Directive("\tmov\tx29, sp"));
        List<Register> registers = registerAllocator.getAllocatedRegisterOrderedList();
        // 分配局部变量空间，确保16字节对齐
        if (frameSize > 0) {
            // 确保frameSize是16字节对齐的
            long alignedFrameSize = (frameSize + 15) & ~15;
            spillOffset = alignedFrameSize;
            alignedFrameSize += registerAllocator.getSpillSlotCount() * 8L;
            alignedFrameSize = (alignedFrameSize + 15) & ~15;
            System.err.println("genPrologue " + alignedFrameSize);
            assembly.add(new Directive("\tsub\tsp, sp, #" + alignedFrameSize));
        }
        // 保存被使用的寄存器
        int counter = 1;
        for (Register register : registers) {
            assembly.add(new Directive("\tstr\t" + register.name() + ", [x29, #-" + (counter++) * 8 + "]"));
        }
        // 保存参数寄存器到栈上，以便longjmp能够正确恢复
        List<Parameter> ps = currentFunction.parameters();
        for (int i = 0; i < ps.size() && i < 8; i++) {
            String reg = "x" + i;
            long offset = registerAllocator.getAllocatedRegisterOrderedList().size() * 8L;
            assembly.add(new Directive("\tstr\t" + reg + ", [x29, #-" + ((i + 1) * 8L + offset) + "]"));
        }
    }

    private void genEpilogue() {
        if (frameSize > 0) {
            // 确保frameSize是16字节对齐的
            long alignedFrameSize = (frameSize + 15) & ~15;
            // assembly.add(new Directive("\tadd\tsp, sp, #" + alignedFrameSize));
            assembly.add(new Directive("\tmov\tsp, x29"));
        }
        // 恢复保存的寄存器
        // todo 需要注意现在的恢复方法不能通过alloca的测试,对吗？
        int counter = 0;
        for (Register register : registerAllocator.getAllocatedRegisterOrderedList()) {
            assembly.add(new Directive("\tldr\t" + register.name() + ", [x29,-" + (counter + 1) * 8 + "]"));
            counter++;
        }
        assembly.add(new Directive("\tldp\tx29, x30, [sp], #16"));
        assembly.add(new Directive("\tret"));
    }

    /* ====== IR Visitor ====== */

    @Override
    public Void visit(Call e) {
        boolean isVar = e.isStaticCall() &&
                e.function().type().getFunctionType().isVararg();
        List<Expr> args = e.args();
        int total = args.size();
        int fixed = 0;
        if (e.isStaticCall()) {
            fixed = e.function().type().getFunctionType().paramTypes().size();
        }

        // 这里使用sp保存的一个问题在于，如果是调用的alloca，恢复的时候会导致栈破坏
        boolean shouldSaveCallerRegister = !e.isStaticCall() || !e.function().name().equals("alloca");
        if (shouldSaveCallerRegister) {
            assembly.add(new Directive("\tstp\tx9, x10, [sp, #-16]!"));
            assembly.add(new Directive("\tstp\tx11, x12, [sp, #-16]!"));
            assembly.add(new Directive("\tstp\tx13, x14, [sp, #-16]!"));
            assembly.add(new Directive("\tstp\tx15, x16, [sp, #-16]!"));
        }

        int stackArgs = Math.max(0, total - ARG_REGS.length);
        int shadow = isVar ? 8 * total : 0;
        int temp = total * 8;
        int block = shadow + stackArgs * 8 + temp;
        block = (block + 15) & ~15;

        if (block > 0)
            assembly.add(new Directive("\tsub\tsp, sp, #" + block));

        int tempBase = shadow + stackArgs * 8;

        // Pass1: L->R
        for (int i = 0; i < total; ++i) {
            args.get(i).accept(this); // x0
            assembly.add(new Directive("\tstr\tx0, [sp, #" + (tempBase + i * 8L) + "]"));
        }
        Register tempRegister = registerAllocator.allocateTempRegister();
        // Pass2: L->R
        for (int i = 0; i < total; ++i) {
            long src = tempBase + i * 8L;
            if (i == 0) {
                assembly.add(new Directive("\tldr\tx0, [sp, #" + src + "]"));
            } else {
                assembly.add(new Directive("\tldr\t" + tempRegister + ", [sp, #" + src + "]"));
            }

            String dst;
            if (i < ARG_REGS.length) {
                dst = ARG_REGS[i].toString();
                if (i != 0) {
                    assembly.add(new Directive("\tmov\t" + dst + ", " + tempRegister));
                }
            } else {
                dst = tempRegister.toString();
            }

            // 可变参数：把所有可变参数push到shadow区域
            if (isVar && i >= fixed) {
                long sh = (i - fixed) * 8L;
                assembly.add(new Directive("\tstr\t" + dst + ", [sp, #" + sh + "]"));
            }
            if (i >= ARG_REGS.length) {
                long off = shadow + (long) (i - ARG_REGS.length) * 8;
                assembly.add(new Directive("\tstr\t" + dst + ", [sp, #" + off + "]"));
            }
        }
        registerAllocator.releaseTempRegister(tempRegister);
        if (e.isStaticCall()) {
            // 对于变长参数函数，我们需要特殊处理va_init调用
            if (e.function().name().equals("va_init")) {
                // va_init期望接收当前栈指针的值
                assembly.add(new Directive("\tmov\tx0, sp"));
            }
            assembly.add(new Directive("\tbl\t" + e.function().callingSymbol().toSource()));
        } else {
            // 对于间接函数调用，需要将函数地址加载到不同的寄存器，避免覆盖x0中的参数
            e.expr().accept(this); // callee -> x0
            tempRegister = allocateTempRegisterWithSpill();
            assembly.add(new Directive("\tmov\t" + tempRegister + ", x0")); // 将函数地址移动到CALL_TMP
            // 现在需要将第一个参数重新加载到x0
            // todo 严格来说这里是有语法错误的，这里会导致第一个表达式执行两次
            if (!e.args().isEmpty()) {
                e.args().get(0).accept(this); // 重新加载第一个参数到x0
            }
            assembly.add(new Directive("\tblr\t" + tempRegister)); // 使用CALL_TMP调用函数
            registerAllocator.releaseTempRegister(tempRegister);
        }

        if (block > 0)
            assembly.add(new Directive("\tadd\tsp, sp, #" + block));
        if (shouldSaveCallerRegister) {
            // 恢复caller-saved寄存器，使用ldp保持16字节对齐
            assembly.add(new Directive("\tldp\tx15, x16, [sp], #16"));
            assembly.add(new Directive("\tldp\tx13, x14, [sp], #16"));
            assembly.add(new Directive("\tldp\tx11, x12, [sp], #16"));
            assembly.add(new Directive("\tldp\tx9, x10, [sp], #16"));
        }

        return null;
    }

    @Override
    public Void visit(Assign s) {
        // 检查LHS是否是Var类型，且已分配寄存器
        if (s.lhs() instanceof Var) {
            Var var = (Var) s.lhs();
            Entity ent = var.entity();
            net.loveruby.cflat.asm.Register reg = registerMap.get(ent);

            if (reg != null) {
                // 变量在寄存器中，直接将RHS的结果放到目标寄存器
                System.err.println("Storing to " + ent.name() + " in register " + reg);

                // 特殊处理：如果RHS是Int，直接放到目标寄存器
                if (s.rhs() instanceof Int) {
                    visitIntToRegister((Int) s.rhs(), reg.toString());
                } else {
                    // 其他情况，先计算到x0，然后移动到目标寄存器
                    s.rhs().accept(this);
                    assembly.add(new Directive("\tmov\t" + reg + ", x0"));
                }
                return null;
            } else if (spillOffsets.containsKey(ent)) {
                // 变量溢出到栈上，计算RHS后存储 - 根据变量类型使用正确的存储指令
                System.err.println("Storing to " + ent.name() + " in spill offset " + spillOffsets.get(ent));
                s.rhs().accept(this);
                long offset = spillOffsets.get(ent);

                // 根据变量类型使用正确的存储指令，避免统一使用64位指令导致的数据覆盖问题
                long sz = s.lhs().type().size();
                if (sz == 8) {
                    // 64位变量：直接使用64位存储指令
                    assembly.add(new Directive("\tstr\tx0, [x29, #" + offset + "]"));
                } else if (sz == 4) {
                    // 32位变量：使用32位存储指令
                    assembly.add(new Directive("\tstr\tw0, [x29, #" + offset + "]"));
                } else if (sz == 2) {
                    // 16位变量：使用16位存储指令
                    assembly.add(new Directive("\tstrh\tw0, [x29, #" + offset + "]"));
                } else if (sz == 1) {
                    // 8位变量：使用8位存储指令
                    assembly.add(new Directive("\tstrb\tw0, [x29, #" + offset + "]"));
                } else {
                    errorHandler.error("unsupported store size: " + sz);
                }
                return null;
            }
        }

        // 参考x86的实现，使用虚拟栈避免寄存器冲突
        if (s.lhs() instanceof Var) {
            // 如果寄存器中没有，溢出中也没有计算，那说明这个变量应该是全局变量
            // 对于Var类型的LHS，直接计算RHS并存储
            s.rhs().accept(this);
            Register temp = allocateTempRegisterWithSpill();
            assembly.add(new Directive("\tmov\t" + temp + ", x0"));
            storeToVar((Var) s.lhs(), temp);
            releaseTempRegisterWithRestore(temp);
            return null;
        }

        // 1) 先算 RHS，值在 x0
        s.rhs().accept(this);
        Register temp = allocateTempRegisterWithSpill();
        Register temp2 = allocateTempRegisterWithSpill();
        assembly.add(new Directive("\tmov\t" + temp + ", x0")); // 保存值

        long sz = s.lhs().type().size(); // 1/2/4/8
        // 生成地址到 x11
        if (s.lhs() instanceof Addr) {
            addrOfEntityInto(((Addr) s.lhs()).entity(), temp2.name());
        } else if (s.lhs() instanceof Mem) {
            evalAddressInto(((Mem) s.lhs()).expr(), temp2.name());
        } else {
            errorHandler.error("unsupported LHS in Assign");
            return null;
        }

        // 根据大小选择指令/寄存器宽度
        String xsrc = temp.toString(); // x9
        String wsrc = "w" + temp.name().substring(1); // w9

        if (sz == 8) {
            assembly.add(new Directive("\tstr\t" + xsrc + ", [" + temp2.name() + "]"));
        } else if (sz == 4) {
            assembly.add(new Directive("\tstr\t" + wsrc + ", [" + temp2.name() + "]"));
        } else if (sz == 2) {
            assembly.add(new Directive("\tstrh\t" + wsrc + ", [" + temp2.name() + "]"));
        } else if (sz == 1) {
            assembly.add(new Directive("\tstrb\t" + wsrc + ", [" + temp2.name() + "]"));
        } else {
            errorHandler.error("unsupported store size: " + sz);
        }
        releaseTempRegisterWithRestore(temp);
        releaseTempRegisterWithRestore(temp2);
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
        // 对于Jump指令，使用向前跳转
        assembly.add(new Directive("\tb\t" + s.label().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    @Override
    public Void visit(Switch s) {
        s.cond().accept(this);
        // 条件值现在在 x0 中
        for (Case c : s.cases()) {
            // 将 case 值加载到临时寄存器
            Register tmp = allocateTempRegisterWithSpill();
            materializeImmediate64(tmp.name(), c.value, false);
            // 比较条件值和 case 值
            assembly.add(new Directive("\tcmp\tx0, " + tmp));
            // 如果相等，跳转到对应的标签
            assembly.add(new Directive("\tbeq\t" + c.label.symbol().toSource(labelSymbols) + "f"));
            releaseTempRegisterWithRestore(tmp);
        }
        // 如果没有匹配的 case，跳转到 default 标签
        assembly.add(new Directive("\tb\t" + s.defaultLabel().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    @Override
    public Void visit(LabelStmt s) {
        assembly.add(new Directive(s.label().symbol().toSource(labelSymbols) + "f:"));
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
        Register tmp0 = allocateTempRegisterWithSpill();
        // assembly.add(new Directive("\tstr\tx0, [sp, #-16]!")); // 将右操作数压栈
        assembly.add(new Directive("\tmov\t" + tmp0.name() + ", x0"));
        e.left().accept(this);
        // assembly.add(new Directive("\tldr\t" + tmp0 + ", [sp], #16")); //
        // 将右操作数出栈到TMP0

        switch (e.op()) {
            case ADD:
                assembly.add(new Directive("\tadd\tx0, x0, " + tmp0));
                break;
            case SUB:
                assembly.add(new Directive("\tsub\tx0, x0, " + tmp0));
                break;
            case MUL:
                assembly.add(new Directive("\tmul\tx0, x0, " + tmp0));
                break;
            case S_DIV:
                assembly.add(new Directive("\tsdiv\tx0, x0, " + tmp0));
                break;
            case S_MOD:
                Register tempRegister = allocateTempRegisterWithSpill();
                assembly.add(new Directive("\tsdiv\t" + tempRegister + ", x0, " + tmp0));
                assembly.add(new Directive("\tmul\t" + tempRegister + ", " + tempRegister + ", " + tmp0));
                assembly.add(new Directive("\tsub\tx0, x0, " + tempRegister));
                releaseTempRegisterWithRestore(tempRegister);
                break;
            case BIT_AND:
                assembly.add(new Directive("\tand\tx0, x0, " + tmp0));
                break;
            case BIT_OR:
                assembly.add(new Directive("\torr\tx0, x0, " + tmp0));
                break;
            case BIT_XOR:
                assembly.add(new Directive("\teor\tx0, x0, " + tmp0));
                break;
            case BIT_LSHIFT:
                assembly.add(new Directive("\tlsl\tx0, x0, " + tmp0));
                break;
            case ARITH_RSHIFT:
                assembly.add(new Directive("\tasr\tx0, x0, " + tmp0));
                break;
            case EQ:
                assembly.add(new Directive("\tcmp\tx0, " + tmp0));
                assembly.add(new Directive("\tcset\tx0, eq"));
                break;
            case NEQ:
                assembly.add(new Directive("\tcmp\tx0, " + tmp0));
                assembly.add(new Directive("\tcset\tx0, ne"));
                break;
            case S_LT:
                assembly.add(new Directive("\tcmp\tx0, " + tmp0));
                assembly.add(new Directive("\tcset\tx0, lt"));
                break;
            case S_LTEQ:
                assembly.add(new Directive("\tcmp\tx0, " + tmp0));
                assembly.add(new Directive("\tcset\tx0, le"));
                break;
            case S_GT:
                assembly.add(new Directive("\tcmp\tx0, " + tmp0));
                assembly.add(new Directive("\tcset\tx0, gt"));
                break;
            case S_GTEQ:
                assembly.add(new Directive("\tcmp\tx0, " + tmp0));
                assembly.add(new Directive("\tcset\tx0, ge"));
                break;
            default:
                // 保持x0不变
                break;
        }
        releaseTempRegisterWithRestore(tmp0);
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
        Register temp = allocateTempRegisterWithSpill();
        evalAddressInto(e.expr(), temp.name());
        String tempRef = "[" + temp.name() + "]";
        long sz = e.type().size();

        if (sz == 1) {
            // 读 1 字节并做有符号扩展成 64 位
            assembly.add(new Directive("\tldrb\tw0, " + tempRef));
            assembly.add(new Directive("\tsxtb\tx0, w0"));
        } else if (sz == 2) {
            assembly.add(new Directive("\tldrh\tw0, " + tempRef));
            assembly.add(new Directive("\tsxth\tx0, w0"));
        } else if (sz == 4) {
            assembly.add(new Directive("\tldr\tw0, " + tempRef));
            assembly.add(new Directive("\tsxtw\tx0, w0"));
        } else if (sz == 8) {
            assembly.add(new Directive("\tldr\tx0, " + tempRef));
        } else {
            errorHandler.error("unsupported load size: " + sz);
        }
        releaseTempRegisterWithRestore(temp);
        return null;
    }

    @Override
    public Void visit(Var e) {
        Entity ent = e.entity();
        net.loveruby.cflat.asm.Register reg = registerMap.get(ent);

        if (reg != null) {
            // 变量在寄存器中，直接移动到x0
            System.err.println("Loading " + ent.name() + " from register " + reg);
            assembly.add(new Directive("\tmov\tx0, " + reg));
        } else if (spillOffsets.containsKey(ent)) {
            // 变量溢出到栈上，从栈加载 - 根据变量类型使用正确的加载指令
            long offset = spillOffsets.get(ent);
            System.err.println("Loading " + ent.name() + " from spill offset " + offset);

            // 根据变量类型使用正确的加载指令，避免统一使用64位指令导致的数据覆盖问题
            long sz = e.type().size();
            if (sz == 8) {
                // 64位变量：直接使用64位加载指令
                assembly.add(new Directive("\tldr\tx0, [x29, #" + offset + "]"));
            } else if (sz == 4) {
                // 32位变量：使用32位加载指令，然后有符号扩展到64位
                assembly.add(new Directive("\tldr\tw0, [x29, #" + offset + "]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            } else if (sz == 2) {
                // 16位变量：使用16位加载指令，然后有符号扩展到64位
                assembly.add(new Directive("\tldrh\tw0, [x29, #" + offset + "]"));
                assembly.add(new Directive("\tsxth\tx0, w0"));
            } else if (sz == 1) {
                // 8位变量：使用8位加载指令，然后有符号扩展到64位
                assembly.add(new Directive("\tldrb\tw0, [x29, #" + offset + "]"));
                assembly.add(new Directive("\tsxtb\tx0, w0"));
            } else {
                errorHandler.error("unsupported load size: " + sz);
            }
        } else {
            // 使用原有的栈访问方法
            System.err.println("Loading " + ent.name() + " from stack");
            loadFromVar(e);
        }
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

    // 新增方法：将整数常量放到指定寄存器
    private void visitIntToRegister(Int e, String targetReg) {
        long v = e.value();
        if (e.type().size() == 4) {
            assembly.add(new Directive("\tmov\tw" + targetReg.substring(1) + ", #" + (v & 0xFFFFFFFFL)));
            assembly.add(new Directive("\tsxtw\t" + targetReg + ", w" + targetReg.substring(1)));
        } else {
            materializeImmediate64(targetReg, v, false);
        }
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
            // 其他参数在栈上，使用正数偏移
            assembly.add(new Directive("\tadd\tx0, x29, #" + off));
        } else {
            // 使用预设的符号引用
            if (ent.address() != null) {
                // 如果有预设的地址，直接使用
                if (ent.address() instanceof ImmediateValue) {
                    ImmediateValue imm = (ImmediateValue) ent.address();
                    if (imm.expr() instanceof Symbol) {
                        Symbol sym = (Symbol) imm.expr();
                        String symStr = sym.toSource();
                        if (symStr.endsWith("@GOT")) {
                            // 对于@GOT符号，使用@GOTPAGE和@GOTPAGEOFF重定位
                            String baseSym = symStr.substring(0, symStr.length() - 4);
                            assembly.add(new Directive("\tadrp\tx0, " + baseSym + "@GOTPAGE"));
                            assembly.add(new Directive("\tldr\tx0, [x0, " + baseSym + "@GOTPAGEOFF]"));
                        } else if (ent.type().isFunction() && !isDefinedHere(ent)) {
                            // 对于外部函数，即使有预设符号，也要使用@GOT重定位
                            assembly.add(new Directive("\tadrp\tx0, " + symStr + "@GOTPAGE"));
                            assembly.add(new Directive("\tldr\tx0, [x0, " + symStr + "@GOTPAGEOFF]"));
                        } else {
                            // 对于普通符号，使用@PAGE和@PAGEOFF重定位
                            assembly.add(new Directive("\tadrp\tx0, " + symStr + "@PAGE"));
                            assembly.add(new Directive("\tadd\tx0, x0, " + symStr + "@PAGEOFF"));
                        }
                    } else {
                        // 兜底方案
                        String symbolName = "_" + ent.name();
                        if (ent.type().isFunction() && !isDefinedHere(ent)) {
                            // 对于外部函数，使用@GOT重定位
                            assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@GOTPAGE"));
                            assembly.add(new Directive("\tldr\tx0, [x0, " + symbolName + "@GOTPAGEOFF]"));
                        } else {
                            assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@PAGE"));
                            assembly.add(new Directive("\tadd\tx0, x0, " + symbolName + "@PAGEOFF"));
                        }
                    }
                } else {
                    // 兜底方案
                    String symbolName = "_" + ent.name();
                    if (ent.type().isFunction() && !isDefinedHere(ent)) {
                        // 对于外部函数，使用@GOT重定位
                        assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@GOTPAGE"));
                        assembly.add(new Directive("\tldr\tx0, [x0, " + symbolName + "@GOTPAGEOFF]"));
                    } else {
                        assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@PAGE"));
                        assembly.add(new Directive("\tadd\tx0, x0, " + symbolName + "@PAGEOFF"));
                    }
                }
            } else if (ent.memref() != null) {
                // 如果有预设的内存引用，使用它
                if (ent.memref() instanceof DirectMemoryReference) {
                    DirectMemoryReference mem = (DirectMemoryReference) ent.memref();
                    if (mem.value() instanceof Symbol) {
                        Symbol sym = (Symbol) mem.value();
                        String symStr = sym.toSource();
                        if (symStr.endsWith("@GOT")) {
                            // 对于@GOT符号，使用@GOTPAGE和@GOTPAGEOFF重定位
                            String baseSym = symStr.substring(0, symStr.length() - 4);
                            assembly.add(new Directive("\tadrp\tx0, " + baseSym + "@GOTPAGE"));
                            assembly.add(new Directive("\tldr\tx0, [x0, " + baseSym + "@GOTPAGEOFF]"));
                        } else {
                            // 对于普通符号，使用@PAGE和@PAGEOFF重定位
                            assembly.add(new Directive("\tadrp\tx0, " + symStr + "@PAGE"));
                            assembly.add(new Directive("\tadd\tx0, x0, " + symStr + "@PAGEOFF"));
                        }
                    } else {
                        // 兜底方案
                        String symbolName = "_" + ent.name();
                        assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@PAGE"));
                        assembly.add(new Directive("\tadd\tx0, x0, " + symbolName + "@PAGEOFF"));
                    }
                } else {
                    // 兜底方案
                    String symbolName = "_" + ent.name();
                    assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@PAGE"));
                    assembly.add(new Directive("\tadd\tx0, x0, " + symbolName + "@PAGEOFF"));
                }
            } else {
                // 兜底方案：动态生成重定位
                String symbolName = "_" + ent.name();
                if (ent.type().isFunction()) {
                    // 对于外部函数，使用@GOT重定位
                    assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@GOTPAGE"));
                    assembly.add(new Directive("\tldr\tx0, [x0, " + symbolName + "@GOTPAGEOFF]"));
                } else {
                    // 对于外部变量，也使用@GOT重定位
                    assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@GOTPAGE"));
                    assembly.add(new Directive("\tldr\tx0, [x0, " + symbolName + "@GOTPAGEOFF]"));
                }
            }
        }
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
            // 递归计算地址到 dst
            evalAddressInto(((Mem) e).expr(), dst);
            // 对结果再做一次解引用，从 dst 指向的地址加载值到 dst
            long sz = e.type().size();
            if (sz == 1) {
                assembly.add(new Directive("\tldrb\tw0, [" + dst + "]"));
                assembly.add(new Directive("\tsxtb\t" + dst + ", w0"));
            } else if (sz == 2) {
                assembly.add(new Directive("\tldrh\tw0, [" + dst + "]"));
                assembly.add(new Directive("\tsxth\t" + dst + ", w0"));
            } else if (sz == 4) {
                assembly.add(new Directive("\tldr\tw0, [" + dst + "]"));
                assembly.add(new Directive("\tsxtw\t" + dst + ", w0"));
            } else if (sz == 8) {
                assembly.add(new Directive("\tldr\t" + dst + ", [" + dst + "]"));
            } else {
                errorHandler.error("unsupported load size: " + sz);
            }
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
                if (op == Op.ADD)
                    assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + "x0"));
                else
                    assembly.add(new Directive("\tsub\t" + dst + ", " + dst + ", " + "x0"));
                return;
            }
            // 对于其他操作，使用栈操作避免寄存器冲突
            b.right().accept(this);
            assembly.add(new Directive("\tstr\tx0, [sp, #-16]!")); // 将右操作数压栈
            b.left().accept(this);
            Register tempRegister = allocateTempRegisterWithSpill();
            Register tempRegister2 = allocateTempRegisterWithSpill();
            assembly.add(new Directive("\tldr\t" + tempRegister + ", [sp], #16")); // 将右操作数出栈到OFFT

            switch (op) {
                case MUL:
                    assembly.add(new Directive("\tmul\t" + dst + ", x0, " + tempRegister));
                    break;
                case S_DIV:
                    assembly.add(new Directive("\tsdiv\t" + dst + ", x0, " + tempRegister));
                    break;
                case S_MOD:
                    assembly.add(new Directive("\tsdiv\t" + tempRegister2 + ", x0, " + tempRegister));
                    assembly.add(new Directive("\tmul\t" + tempRegister2 + ", " + tempRegister2 + ", " + tempRegister));
                    assembly.add(new Directive("\tsub\t" + dst + ", x0, " + tempRegister));
                    break;
                case BIT_AND:
                    assembly.add(new Directive("\tand\t" + dst + ", x0, " + tempRegister));
                    break;
                case BIT_OR:
                    assembly.add(new Directive("\torr\t" + dst + ", x0, " + tempRegister));
                    break;
                case BIT_XOR:
                    assembly.add(new Directive("\teor\t" + dst + ", x0, " + tempRegister));
                    break;
                case BIT_LSHIFT:
                    assembly.add(new Directive("\tlsl\t" + dst + ", x0, " + tempRegister));
                    break;
                case ARITH_RSHIFT:
                    assembly.add(new Directive("\tasr\t" + dst + ", x0, " + tempRegister));
                    break;
                default:
                    // 对于比较操作，我们不应该在这里处理地址计算
                    errorHandler.error("unsupported binary operation in address calculation: " + op);
                    break;
            }
            releaseTempRegisterWithRestore(tempRegister);
            releaseTempRegisterWithRestore(tempRegister2);
            return;
        }
        // fallback
        e.accept(this); // x0 assumed = address
        if (!"x0".equals(dst))
            assembly.add(new Directive("\tmov\t" + dst + ", x0"));
    }

    /**
     * 计算 Entity 的地址并写入指定寄存器 dst。
     * - 局部变量 / 形参：沿用原先栈偏移逻辑
     * - 当前目标文件内定义的全局符号：使用 PAGE/PAGEOFF
     * - 其它外部符号：必须通过 GOT 先取绝对地址
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
            // 其他参数在栈上，使用正数偏移
            assembly.add(new Directive("\tadd\t" + dst + ", x29, #" + off));
        } else {
            // 使用预设的符号引用
            if (ent.address() != null) {
                // 如果有预设的地址，直接使用
                if (ent.address() instanceof ImmediateValue) {
                    ImmediateValue imm = (ImmediateValue) ent.address();
                    if (imm.expr() instanceof Symbol) {
                        Symbol sym = (Symbol) imm.expr();
                        String symStr = sym.toSource();
                        if (symStr.endsWith("@GOT")) {
                            // 对于@GOT符号，使用@GOTPAGE和@GOTPAGEOFF重定位
                            String baseSym = symStr.substring(0, symStr.length() - 4);
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + baseSym + "@GOTPAGE"));
                            assembly.add(
                                    new Directive("\tldr\t" + dst + ", [" + dst + ", " + baseSym + "@GOTPAGEOFF]"));
                        } else {
                            // 对于普通符号，使用@PAGE和@PAGEOFF重定位
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + symStr + "@PAGE"));
                            assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symStr + "@PAGEOFF"));
                        }
                    } else {
                        // 兜底方案
                        String symbolName = "_" + ent.name();
                        assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@PAGE"));
                        assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symbolName + "@PAGEOFF"));
                    }
                } else {
                    // 兜底方案
                    String symbolName = "_" + ent.name();
                    assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@PAGE"));
                    assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symbolName + "@PAGEOFF"));
                }
            } else if (ent.memref() != null) {
                // 如果有预设的内存引用，使用它
                if (ent.memref() instanceof DirectMemoryReference) {
                    DirectMemoryReference mem = (DirectMemoryReference) ent.memref();
                    if (mem.value() instanceof Symbol) {
                        Symbol sym = (Symbol) mem.value();
                        String symStr = sym.toSource();
                        if (symStr.endsWith("@GOT")) {
                            // 对于@GOT符号，使用@GOTPAGE和@GOTPAGEOFF重定位
                            String baseSym = symStr.substring(0, symStr.length() - 4);
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + baseSym + "@GOTPAGE"));
                            assembly.add(
                                    new Directive("\tldr\t" + dst + ", [" + dst + ", " + baseSym + "@GOTPAGEOFF]"));
                        } else {
                            // 对于普通符号，使用@PAGE和@PAGEOFF重定位
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + symStr + "@PAGE"));
                            assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symStr + "@PAGEOFF"));
                        }
                    } else {
                        // 兜底方案
                        String symbolName = "_" + ent.name();
                        assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@PAGE"));
                        assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symbolName + "@PAGEOFF"));
                    }
                } else {
                    // 兜底方案
                    String symbolName = "_" + ent.name();
                    assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@PAGE"));
                    assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symbolName + "@PAGEOFF"));
                }
            } else {
                // 兜底方案：动态生成重定位
                String symbolName = "_" + ent.name();
                if (ent.type().isFunction()) {
                    // 对于外部函数，使用@GOT重定位
                    assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@GOTPAGE"));
                    assembly.add(new Directive("\tldr\t" + dst + ", [" + dst + ", " + symbolName + "@GOTPAGEOFF]"));
                } else {
                    // 对于外部变量，也使用@GOT重定位
                    assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@GOTPAGE"));
                    assembly.add(new Directive("\tldr\t" + dst + ", [" + dst + ", " + symbolName + "@GOTPAGEOFF]"));
                }
            }
        }
    }

    private void loadFromVar(Var v) {
        Entity ent = v.entity();
        long sz = v.type().size();
        if (registerAllocator.isInRegister(ent)) {
            Register register = registerAllocator.getRegister(ent);
            assembly.add(new Directive("\tmov\tx0, " + register.name()));
        } else if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (sz == 8) {
                assembly.add(new Directive("\tldr\tx0, [x29, #" + off + "]"));
            } else if (sz == 4) {
                assembly.add(new Directive("\tldr\tw0, [x29, #" + off + "]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            } else if (sz == 2) {
                assembly.add(new Directive("\tldrh\tw0, [x29, #" + off + "]"));
                assembly.add(new Directive("\tsxth\tx0, w0"));
            } else if (sz == 1) {
                assembly.add(new Directive("\tldrb\tw0, [x29, #" + off + "]"));
                assembly.add(new Directive("\tsxtb\tx0, w0"));
            } else {
                errorHandler.error("unsupported load size: " + sz);
            }
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            // 对于参数，优先从局部变量位置加载（如果存在）
            if (localVarOffsets.containsKey(ent)) {
                long localOff = localVarOffsets.get(ent);
                if (sz == 8) {
                    assembly.add(new Directive("\tldr\tx0, [x29, #" + localOff + "]"));
                } else if (sz == 4) {
                    assembly.add(new Directive("\tldr\tw0, [x29, #" + localOff + "]"));
                    assembly.add(new Directive("\tsxtw\tx0, w0"));
                } else if (sz == 2) {
                    assembly.add(new Directive("\tldrh\tw0, [x29, #" + localOff + "]"));
                    assembly.add(new Directive("\tsxth\tx0, w0"));
                } else if (sz == 1) {
                    assembly.add(new Directive("\tldrb\tw0, [x29, #" + localOff + "]"));
                    assembly.add(new Directive("\tsxtb\tx0, w0"));
                } else {
                    errorHandler.error("unsupported load size: " + sz);
                }
            } else {
                // 在 ARM64 中，前 8 个参数通过寄存器传递
                // 参数在栈上，从栈加载
                if (sz == 8) {
                    assembly.add(new Directive("\tldr\tx0, [x29, #" + off + "]"));
                } else if (sz == 4) {
                    assembly.add(new Directive("\tldr\tw0, [x29, #" + off + "]"));
                    assembly.add(new Directive("\tsxtw\tx0, w0"));
                } else if (sz == 2) {
                    assembly.add(new Directive("\tldrh\tw0, [x29, #" + off + "]"));
                    assembly.add(new Directive("\tsxth\tx0, w0"));
                } else if (sz == 1) {
                    assembly.add(new Directive("\tldrb\tw0, [x29, #" + off + "]"));
                    assembly.add(new Directive("\tsxtb\tx0, w0"));
                } else {
                    errorHandler.error("unsupported load size: " + sz);
                }
            }
        } else {
            // 对于外部符号，根据类型选择重定位方式
            String symbolName = "_" + ent.name();
            if (ent.type().isFunction()) {
                // 检查函数是否在当前编译单元中定义
                boolean isDefinedInCurrentUnit = false;
                if (currentIR != null) {
                    for (DefinedFunction func : currentIR.definedFunctions()) {
                        if (func.name().equals(ent.name())) {
                            isDefinedInCurrentUnit = true;
                            break;
                        }
                    }
                }

                if (options.isPositionIndependent()) {
                    if (ent.isPrivate() || isDefinedInCurrentUnit) {
                        // 对于静态函数或在当前编译单元中定义的函数，使用@PAGE/@PAGEOFF重定位
                        assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@PAGE"));
                        assembly.add(new Directive("\tadd\tx0, x0, " + symbolName + "@PAGEOFF"));
                    } else {
                        // 对于外部函数，使用@GOT重定位，加载到x16避免覆盖x0中的参数
                        assembly.add(new Directive("\tadrp\tx16, " + symbolName + "@GOTPAGE"));
                        assembly.add(new Directive("\tldr\tx16, [x16, " + symbolName + "@GOTPAGEOFF]"));
                        assembly.add(new Directive("\tmov\tx0, x16"));
                    }
                } else {
                    // 非PIC模式，使用@PAGE/@PAGEOFF重定位
                    assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@PAGE"));
                    assembly.add(new Directive("\tadd\tx0, x0, " + symbolName + "@PAGEOFF"));
                }
            } else {
                // 对于外部变量，使用@GOT重定位
                assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@GOTPAGE"));
                assembly.add(new Directive("\tldr\tx0, [x0, " + symbolName + "@GOTPAGEOFF]"));
            }
            if (sz == 8) {
                assembly.add(new Directive("\tldr\tx0, [x0]"));
            } else if (sz == 4) {
                assembly.add(new Directive("\tldr\tw0, [x0]"));
                assembly.add(new Directive("\tsxtw\tx0, w0"));
            } else if (sz == 2) {
                assembly.add(new Directive("\tldrh\tw0, [x0]"));
                assembly.add(new Directive("\tsxth\tx0, w0"));
            } else if (sz == 1) {
                assembly.add(new Directive("\tldrb\tw0, [x0]"));
                assembly.add(new Directive("\tsxtb\tx0, w0"));
            } else {
                errorHandler.error("unsupported load size: " + sz);
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
            } else if (sz == 4) {
                String w = "w" + src.name().substring(1);
                if (off >= 0) {
                    assembly.add(new Directive("\tstr\t" + w + ", [x29, #" + off + "]"));
                } else {
                    assembly.add(new Directive("\tstr\t" + w + ", [x29, #-" + (-off) + "]"));
                }
            } else if (sz == 2) {
                String w = "w" + src.name().substring(1);
                if (off >= 0) {
                    assembly.add(new Directive("\tstrh\t" + w + ", [x29, #" + off + "]"));
                } else {
                    assembly.add(new Directive("\tstrh\t" + w + ", [x29, #-" + (-off) + "]"));
                }
            } else if (sz == 1) {
                String w = "w" + src.name().substring(1);
                if (off >= 0) {
                    assembly.add(new Directive("\tstrb\t" + w + ", [x29, #" + off + "]"));
                } else {
                    assembly.add(new Directive("\tstrb\t" + w + ", [x29, #-" + (-off) + "]"));
                }
            } else {
                errorHandler.error("unsupported store size: " + sz);
            }
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            // 对于参数，我们需要将其存储到局部变量空间中
            // 首先检查是否已经有局部变量偏移
            if (localVarOffsets.containsKey(ent)) {
                long localOff = localVarOffsets.get(ent);
                if (localOff >= 0) {
                    assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + localOff + "]"));
                } else {
                    assembly.add(new Directive("\tstr\t" + src + ", [x29, #-" + (-localOff) + "]"));
                }
            } else {
                // 如果没有局部变量偏移，使用参数偏移
                assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + off + "]"));
            }
        } else {
            // 对于外部符号，根据类型选择重定位方式
            String symbolName = "_" + ent.name();
            if (ent.type().isFunction()) {
                // 对于外部函数，使用@GOT重定位
                assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@GOTPAGE"));
                assembly.add(new Directive("\tldr\tx0, [x0, " + symbolName + "@GOTPAGEOFF]"));
            } else {
                // 对于外部变量，使用@GOT重定位
                assembly.add(new Directive("\tadrp\tx0, " + symbolName + "@GOTPAGE"));
                assembly.add(new Directive("\tldr\tx0, [x0, " + symbolName + "@GOTPAGEOFF]"));
            }
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

    private boolean isDefinedHere(Entity ent) {
        if (ent.isPrivate())
            return true;
        if (currentIR == null)
            return false;
        // 检查函数
        for (DefinedFunction f : currentIR.definedFunctions()) {
            if (f.name().equals(ent.name()))
                return true;
        }
        // 检查带初始化或未初始化的全局变量
        for (DefinedVariable v : currentIR.definedGlobalVariables()) {
            if (v.name().equals(ent.name()))
                return true;
        }
        for (DefinedVariable v : currentIR.definedCommonSymbols()) {
            if (v.name().equals(ent.name()))
                return true;
        }
        return false; // 其它情况视为外部符号
    }

    /**
     * 分配临时寄存器，如果池为空则处理溢出（支持重复spill）
     */
    private Register allocateTempRegisterWithSpill() {
        Register reg = registerAllocator.allocateTempRegister();
        if (reg == null) {
            // 需要溢出，获取需要溢出的寄存器
            Register spilledReg = registerAllocator.getRegisterToSpill();
            long relativeOffset = registerAllocator.getNextSpillOffset();

            // 临时寄存器溢出使用相对于sp的偏移量，指向栈帧底部
            // 栈帧布局：[sp] -> [局部变量] -> [临时寄存器溢出] -> [寄存器保存] -> [参数]
            long absoluteOffset = spillOffset + relativeOffset; // 相对于sp的偏移量

            // 检查寄存器是否已经被spill过
            int spillDepth = registerAllocator.getSpillDepth(spilledReg);
            System.err.println("allocateTempRegisterWithSpill " + spilledReg.name() + " to " + absoluteOffset
                    + " (spill depth: " + spillDepth + ")");

            // 生成溢出代码：保存寄存器内容到栈
            // 如果偏移量超过ARM64的限制，使用基址寄存器+偏移量的方式
            if (absoluteOffset > 255) {
                // 使用sp作为基址寄存器，偏移量相对于sp
                assembly.add(new Directive("\tstr\t" + spilledReg + ", [sp, #" + relativeOffset + "]"));
            } else {
                // 使用x29作为基址寄存器，偏移量相对于x29
                assembly.add(new Directive("\tstr\t" + spilledReg + ", [x29, #-" + absoluteOffset + "]"));
            }

            // 记录溢出状态（使用相对偏移量，支持重复spill）
            registerAllocator.recordSpill(spilledReg, relativeOffset);

            // 重新分配这个寄存器
            reg = spilledReg;
        } else {
            System.err.println("allocateTempRegisterWithSpill " + reg.name());
        }
        return reg;
    }

    /**
     * 释放临时寄存器，如果被溢出则恢复（支持重复spill）
     */
    private void releaseTempRegisterWithRestore(Register register) {
        // 检查这个寄存器是否被溢出
        Long relativeOffset = registerAllocator.getSpillOffset(register);
        if (relativeOffset != null) {
            // 临时寄存器溢出使用相对于sp的偏移量
            long absoluteOffset = spillOffset + relativeOffset; // 相对于sp的偏移量

            // 获取spill深度信息
            int spillDepth = registerAllocator.getSpillDepth(register);
            System.err.println("releaseTempRegisterWithRestore " + register.name() + " from " + absoluteOffset
                    + " (spill depth: " + spillDepth + ")");

            // 生成恢复代码：从栈上恢复寄存器内容
            // 如果偏移量超过ARM64的限制，使用基址寄存器+偏移量的方式
            if (absoluteOffset > 255) {
                // 使用sp作为基址寄存器，偏移量相对于sp
                assembly.add(new Directive("\tldr\t" + register + ", [sp, #" + relativeOffset + "]"));
            } else {
                // 使用x29作为基址寄存器，偏移量相对于x29
                assembly.add(new Directive("\tldr\t" + register + ", [x29, #-" + absoluteOffset + "]"));
            }

            // 清理溢出状态（弹出栈顶）
            registerAllocator.clearSpill(register);
        }

        // 释放寄存器
        registerAllocator.releaseTempRegister(register);
    }

}
