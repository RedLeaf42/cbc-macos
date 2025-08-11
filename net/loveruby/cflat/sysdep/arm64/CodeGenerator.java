package net.loveruby.cflat.sysdep.arm64;

import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.ir.*;
import net.loveruby.cflat.type.FloatType;
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
public class CodeGenerator
        implements net.loveruby.cflat.sysdep.CodeGenerator, RegisterAwareVisitor<Void, Void> {

    /* ====== Config ====== */
    private final AssemblyCode assembly;
    private final ErrorHandler errorHandler;
    @SuppressWarnings("FieldCanBeLocal")
    private final net.loveruby.cflat.asm.Type naturalType;
    private final net.loveruby.cflat.sysdep.CodeGeneratorOptions options;

    private static final Register[] ARG_REGS = {
            Register.X0, Register.X1, Register.X2, Register.X3,
            Register.X4, Register.X5, Register.X6, Register.X7
    };

    private static final Register[] FLOAT_ARG_REGS = {
            Register.D0, Register.D1, Register.D2, Register.D3,
            Register.D4, Register.D5, Register.D6, Register.D7
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
    private final RegisterAllocator registerAllocator;
    private final Map<Entity, net.loveruby.cflat.asm.Register> registerMap = new HashMap<>();
    private final Map<Entity, Long> spillOffsets = new HashMap<>();

    // 当前正在处理的语句（用于活跃变量分析）
    private Stmt currentStmt;

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
        System.err.println("frameSize original " + frameSize + " registerSize=" + allocatedRegisterSize);
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
            if (registerAllocator.isInRegister(var)) {
                System.err.println("InRegister " + var.name());
                registerMap.put(var, registerAllocator.getRegister(var));
            } else if (registerAllocator.isSpilled(var)) {
                System.err.println("InSpill " + var.name());
                spillOffsets.put(var, registerAllocator.getSpillOffset(var));
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
        for (Stmt s : func.ir()) {
            currentStmt = s;
            visit(s);
        }
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
            // spillOffset必须是一个可以安全开始的存储点，换言之它必须和之前的alignedFrameSize必须有8字节的差距
            alignedFrameSize += 8; // 为什么要偏移8，是因为这个点才是安全的点，不会覆盖之前的数据,因为spillOffset是从0开始计算的
            spillOffset = alignedFrameSize;
            alignedFrameSize += registerAllocator.getSpillSlotCount() * 8L;
            alignedFrameSize = (alignedFrameSize + 15) & ~15;
            System.err.println("genPrologue " + alignedFrameSize + "spillOffset: " + spillOffset);
            assembly.add(new Directive("\tsub\tsp, sp, #" + alignedFrameSize));
        }
        // 保存被使用的寄存器
        int counter = 1;
        for (Register register : registers) {
            if (register.name().startsWith("d")) {
                // 浮点数寄存器使用str指令
                assembly.add(new Directive("\tstr\t" + register.name() + ", [x29, #-" + (counter++) * 8 + "]"));
            } else {
                // 整数寄存器使用str指令
                assembly.add(new Directive("\tstr\t" + register.name() + ", [x29, #-" + (counter++) * 8 + "]"));
            }
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
        // 恢复保存的寄存器
        // todo 需要注意现在的恢复方法不能通过alloca的测试,对吗？
        int counter = 0;
        for (Register register : registerAllocator.getAllocatedRegisterOrderedList()) {
            if (register.name().startsWith("d")) {
                // 浮点数寄存器使用ldr指令
                assembly.add(new Directive("\tldr\t" + register.name() + ", [x29,-" + (counter + 1) * 8 + "]"));
            } else {
                // 整数寄存器使用ldr指令
                assembly.add(new Directive("\tldr\t" + register.name() + ", [x29,-" + (counter + 1) * 8 + "]"));
            }
            counter++;
        }
        // 恢复栈指针
        assembly.add(new Directive("\tmov\tsp, x29"));
        assembly.add(new Directive("\tldp\tx29, x30, [sp], #16"));
        assembly.add(new Directive("\tret"));
    }

    /* ====== IR Visitor ====== */

    public Void visit(Call e) {
        boolean isVar = e.isStaticCall() &&
                e.function().type().getFunctionType().isVararg();
        List<Expr> args = e.args();
        int total = args.size();
        int fixed = 0;
        if (e.isStaticCall()) {
            fixed = e.function().type().getFunctionType().paramTypes().size();
        }

        // 优化：只保存实际使用到的caller-saved寄存器
        boolean shouldSaveCallerRegister = !e.isStaticCall() || !e.function().name().equals("alloca");
        Set<Register> usedCallerSaved = new HashSet<>();
        if (shouldSaveCallerRegister) {
            // 获取在函数调用点实际活跃的caller-saved寄存器
            usedCallerSaved = registerAllocator.getLiveCallerSavedRegisters(currentStmt);
            System.err.println("Live caller-saved registers at call site: "
                    + usedCallerSaved.stream().map(Register::name).collect(Collectors.joining(", ")));

            // 按寄存器对保存，保持16字节对齐
            saveCallerSavedRegisters(usedCallerSaved);
        }

        int stackArgs = Math.max(0, total - ARG_REGS.length);
        int shadow = isVar ? 8 * (total - fixed) : 0;
        int temp = total * 8;
        int block = shadow + stackArgs * 8 + temp;
        block = (block + 15) & ~15;

        if (block > 0)
            assembly.add(new Directive("\tsub\tsp, sp, #" + block));

        int tempBase = shadow + stackArgs * 8;

        // Pass1: L->R - 处理浮点数和整数参数
        for (int i = 0; i < total; ++i) {
            Expr arg = args.get(i);
            if (isFloatOperand(arg)) {
                // 浮点数参数：直接加载到浮点寄存器
                if (i < FLOAT_ARG_REGS.length) {
                    arg.accept(this, FLOAT_ARG_REGS[i]);
                } else {
                    // 浮点数参数过多，需要存储到栈上
                    arg.accept(this, Register.D0); // 临时使用d0
                    assembly.add(new Directive("\tstr\td0, [sp, #" + (tempBase + i * 8L) + "]"));
                }
            } else {
                // 整数参数：加载到整数寄存器
                if (i < ARG_REGS.length) {
                    arg.accept(this, ARG_REGS[i]);
                } else {
                    // 整数参数过多，需要存储到栈上
                    arg.accept(this, Register.X0); // 临时使用x0
                    assembly.add(new Directive("\tstr\tx0, [sp, #" + (tempBase + i * 8L) + "]"));
                }
            }
        }
        if (e.isStaticCall()) {
            // 对于变长参数函数，我们需要特殊处理va_init调用
            if (e.function().name().equals("va_init")) {
                // va_init期望接收当前栈指针的值
                assembly.add(new Directive("\tmov\tx0, sp"));
            }
            assembly.add(new Directive("\tbl\t" + e.function().callingSymbol().toSource()));
        } else {
            // 对于间接函数调用，需要将函数地址加载到不同的寄存器，避免覆盖x0中的参数
            e.expr().accept(this, Register.X0); // callee -> x0
            TempRegisterAllocationContext context = new TempRegisterAllocationContext(
                    registerAllocator.getSpillSlotCount(),
                    "Call");
            Register tempRegister = allocateTempRegisterWithSpill(context, "tmp pointer");
            assembly.add(new Directive("\tmov\t" + tempRegister + ", x0")); // 将函数地址移动到临时寄存器
            // 现在需要将第一个参数重新加载到x0
            if (!e.args().isEmpty()) {
                e.args().get(0).accept(this, Register.X0); // 重新加载第一个参数到x0
            }
            assembly.add(new Directive("\tblr\t" + tempRegister)); // 使用临时寄存器调用函数
            releaseTempRegisterWithRestore(tempRegister, context);
        }

        if (block > 0)
            assembly.add(new Directive("\tadd\tsp, sp, #" + block));
        if (shouldSaveCallerRegister) {
            // 恢复caller-saved寄存器，使用ldp保持16字节对齐
            restoreCallerSavedRegisters(usedCallerSaved);
        }

        return null;
    }

    @Override
    public Void visit(Assign s) {
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(registerAllocator.getSpillSlotCount(),
                "Assign");
        // 检查LHS是否是Var类型，且已分配寄存器
        if (s.lhs() instanceof Var var) {
            Entity ent = var.entity();
            net.loveruby.cflat.asm.Register reg = registerMap.get(ent);

            if (reg != null) {
                // 变量在寄存器中，直接将RHS的结果放到目标寄存器
                System.err.println("Storing to " + ent.name() + " in register " + reg);

                // 使用RegisterAwareVisitor，直接将RHS计算到目标寄存器
                Register targetReg = getRegisterByName(reg.toString());
                s.rhs().accept(this, targetReg);
                return null;
            } else if (spillOffsets.containsKey(ent)) {
                // 变量溢出到栈上，计算RHS后存储 - 根据变量类型使用正确的存储指令
                System.err.println("Storing to " + ent.name() + " in spill offset " + spillOffsets.get(ent));
                // 根据变量类型选择目标寄存器
                if (ent.type().isFloat()) {
                    // 浮点数变量使用浮点数寄存器
                    s.rhs().accept(this, Register.D0);
                    long offset = spillOffsets.get(ent);
                    assembly.add(new Directive("\tstr\td0, [x29, #" + offset + "]"));
                } else {
                    // 整数变量使用整数寄存器
                    s.rhs().accept(this, Register.X0);
                    long offset = spillOffsets.get(ent);
                    // 整数变量根据大小使用不同的存储指令
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
                }
                return null;
            }
        }

        // 参考x86的实现，使用虚拟栈避免寄存器冲突
        if (s.lhs() instanceof Var) {
            // 如果寄存器中没有，溢出中也没有计算，那说明这个变量应该是全局变量
            // 对于Var类型的LHS，直接计算RHS并存储
            Var var = (Var) s.lhs();
            if (var.entity().type().isFloat()) {
                // 浮点数变量使用浮点数寄存器
                s.rhs().accept(this, Register.D0);
                Register temp = allocateFloatTempRegisterWithSpill(context, "assign rhs save");
                assembly.add(new Directive("\tfmov\t" + temp + ", d0"));
                storeToVar(var, temp);
                releaseTempRegisterWithRestore(temp, context);
            } else {
                // 整数变量使用整数寄存器
                s.rhs().accept(this, Register.X0);
                Register temp = allocateTempRegisterWithSpill(context, "assign rhs save");
                assembly.add(new Directive("\tmov\t" + temp + ", x0"));
                storeToVar(var, temp);
                releaseTempRegisterWithRestore(temp, context);
            }
            return null;
        }

        // 1) 先算 RHS，暂时使用整数寄存器
        Register temp = allocateTempRegisterWithSpill(context, "assign tmp0");
        Register temp2 = allocateTempRegisterWithSpill(context, "assign tmp1"); // 地址计算总是用整数寄存器

        // 暂时使用整数寄存器，后续可以扩展浮点数支持
        s.rhs().accept(this, Register.X0);
        assembly.add(new Directive("\tmov\t" + temp + ", x0")); // 保存值

        long sz = s.lhs().type().size(); // 1/2/4/8
        // 生成地址到 temp2，避免覆盖temp中的值
        if (s.lhs() instanceof Addr) {
            addrOfEntityInto(((Addr) s.lhs()).entity(), temp2.name());
        } else if (s.lhs() instanceof Mem) {
            evalAddressInto(((Mem) s.lhs()).expr(), temp2);
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
        releaseTempRegisterWithRestore(temp, context);
        releaseTempRegisterWithRestore(temp2, context);
        context.checkState();
        return null;
    }

    public Void visit(ExprStmt s) {
        s.expr().accept(this, Register.X0);
        return null;
    }

    public Void visit(CJump s) {
        s.cond().accept(this, Register.X0);
        assembly.add(new Directive("\tcmp\tx0, #0"));
        assembly.add(new Directive("\tb.ne\t" + s.thenLabel().symbol().toSource(labelSymbols) + "f"));
        assembly.add(new Directive("\tb\t" + s.elseLabel().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    public Void visit(Jump s) {
        // 对于Jump指令，使用向前跳转
        assembly.add(new Directive("\tb\t" + s.label().symbol().toSource(labelSymbols) + "f"));
        return null;
    }

    public Void visit(Switch s) {
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(registerAllocator.getSpillSlotCount(),
                "Switch");
        s.cond().accept(this, Register.X0);
        // 条件值现在在 x0 中
        for (Case c : s.cases()) {
            // 将 case 值加载到临时寄存器
            Register tmp = allocateTempRegisterWithSpill(context, "switch case");
            materializeImmediate64(tmp.name(), c.value, false);
            // 比较条件值和 case 值
            assembly.add(new Directive("\tcmp\tx0, " + tmp));
            // 如果相等，跳转到对应的标签
            assembly.add(new Directive("\tbeq\t" + c.label.symbol().toSource(labelSymbols) + "f"));
            releaseTempRegisterWithRestore(tmp, context);
        }
        // 如果没有匹配的 case，跳转到 default 标签
        assembly.add(new Directive("\tb\t" + s.defaultLabel().symbol().toSource(labelSymbols) + "f"));
        context.checkState();
        return null;
    }

    public Void visit(LabelStmt s) {
        assembly.add(new Directive(s.label().symbol().toSource(labelSymbols) + "f:"));
        return null;
    }

    public Void visit(Return s) {
        if (s.expr() != null) {
            // 使用RegisterAwareVisitor，计算到x0
            s.expr().accept(this, Register.X0);
        } else {
            assembly.add(new Directive("\tmov\tx0, #0"));
        }
        assembly.add(new Directive("\tb\t.L" + currentFunction.name() + "_epilogue"));
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

    private void evalAddressInto(Expr e, Register dstRegister) {
        String dst = dstRegister.name();
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(
                registerAllocator.getSpillSlotCount() - 1, "evalAddressInto");
        context.recordRegisterAllocation(dstRegister);
        if (e instanceof Addr) {
            addrOfEntityInto(((Addr) e).entity(), dst);
            return;
        }
        if (e instanceof Var) {
            // 对于Var，我们需要读取变量的值，而不是计算变量的地址
            // 因为Var可能存储的是一个地址（比如@tmp0存储&a[0]）
            loadFromVar((Var) e, dst);
            return;
        }
        if (e instanceof Mem) {
            // 递归计算地址到 dst
            evalAddressInto(((Mem) e).expr(), dstRegister);
            // 对结果再做一次解引用，从 dst 指向的地址加载值到 dst
            long sz = e.type().size();
            if (sz == 1) {
                assembly.add(new Directive("\tldrb\tw0, [" + dst + "]"));
                assembly.add(new Directive("\tsxtb\t" + dst + ", w0"));
            } else if (sz == 2) {
                assembly.add(new Directive("\tldrh\tw0, [" + dst + "]"));
                assembly.add(new Directive("\tsxth\t" + dst + ", w0"));
            } else if (sz == 4) {
                assembly.add(new Directive("\tldr\t" + dstRegister.bit32Name() + " [" + dst + "]"));
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
        if (e instanceof Bin b) {
            Op op = b.op();
            if (op == Op.ADD || op == Op.SUB) {
                evalAddressInto(b.left(), dstRegister); // base -> dst
                // 使用临时寄存器计算右边的表达式，避免寄存器冲突
                b.right().accept(this, Register.X0);
                if (op == Op.ADD)
                    assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + "x0"));
                else
                    assembly.add(new Directive("\tsub\t" + dst + ", " + dst + ", " + "x0"));
                return;
            }
            // 对于其他操作，使用临时寄存器避免寄存器冲突
            b.right().accept(this, Register.X0);
            Register tempRegister = allocateTempRegisterWithSpill(context, "evalAddressInto other");
            if (tempRegister.name().equals(dst)) {
                throw new IllegalStateException(("tmpRegister wrong !! " + dst));
            }
            assembly.add(new Directive("\tmov\t" + tempRegister + ", x0")); // 将右操作数保存到临时寄存器
            b.left().accept(this, Register.X0);

            switch (op) {
                case MUL:
                    assembly.add(new Directive("\tmul\t" + dst + ", x0, " + tempRegister));
                    break;
                case S_DIV:
                    assembly.add(new Directive("\tsdiv\t" + dst + ", x0, " + tempRegister));
                    break;
                case S_MOD:
                    Register tempRegister2 = allocateTempRegisterWithSpill(context, "evalAddressInto mod");
                    if (tempRegister2.name().equals(dst) || tempRegister2.name().equals(tempRegister.name())) {
                        throw new IllegalStateException(("tmpRegister wrong stage 2!! " + dst));
                    }
                    assembly.add(new Directive("\tsdiv\t" + tempRegister2 + ", x0, " + tempRegister));
                    assembly.add(new Directive("\tmul\t" + tempRegister2 + ", " + tempRegister2 + ", " + tempRegister));
                    assembly.add(new Directive("\tsub\t" + dst + ", x0, " + tempRegister));
                    releaseTempRegisterWithRestore(tempRegister2, context);
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
            releaseTempRegisterWithRestore(tempRegister, context);
            context.checkState();
            return;
        }
        // fallback
        e.accept(this, Register.X0); // x0 assumed = address
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
                if (ent.address() instanceof ImmediateValue imm) {
                    if (imm.expr() instanceof Symbol sym) {
                        String symStr = sym.toSource();
                        if (symStr.endsWith("@GOT")) {
                            // 对于@GOT符号，使用@GOTPAGE和@GOTPAGEOFF重定位
                            String baseSym = symStr.substring(0, symStr.length() - 4);
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + baseSym + "@GOTPAGE"));
                            assembly.add(
                                    new Directive("\tldr\t" + dst + ", [" + dst + ", " + baseSym + "@GOTPAGEOFF]"));
                        } else if (ent.type().isFunction() && !isDefinedHere(ent)) {
                            // 外部函数：通过GOT取地址
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + symStr + "@GOTPAGE"));
                            assembly.add(new Directive("\tldr\t" + dst + ", [" + dst + ", " + symStr + "@GOTPAGEOFF]"));
                        } else {
                            // 其它：PAGE/PAGEOFF
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + symStr + "@PAGE"));
                            assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symStr + "@PAGEOFF"));
                        }
                    } else {
                        // 兜底方案
                        String symbolName = "_" + ent.name();
                        if (ent.type().isFunction() && !isDefinedHere(ent)) {
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@GOTPAGE"));
                            assembly.add(
                                    new Directive("\tldr\t" + dst + ", [" + dst + ", " + symbolName + "@GOTPAGEOFF]"));
                        } else {
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@PAGE"));
                            assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symbolName + "@PAGEOFF"));
                        }
                    }
                } else {
                    // 兜底方案
                    String symbolName = "_" + ent.name();
                    if (ent.type().isFunction() && !isDefinedHere(ent)) {
                        assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@GOTPAGE"));
                        assembly.add(new Directive("\tldr\t" + dst + ", [" + dst + ", " + symbolName + "@GOTPAGEOFF]"));
                    } else {
                        assembly.add(new Directive("\tadrp\t" + dst + ", " + symbolName + "@PAGE"));
                        assembly.add(new Directive("\tadd\t" + dst + ", " + dst + ", " + symbolName + "@PAGEOFF"));
                    }
                }
            } else if (ent.memref() != null) {
                // 如果有预设的内存引用，使用它
                if (ent.memref() instanceof DirectMemoryReference mem) {
                    if (mem.value() instanceof Symbol sym) {
                        String symStr = sym.toSource();
                        if (symStr.endsWith("@GOT")) {
                            // 对于@GOT符号，使用@GOTPAGE和@GOTPAGEOFF重定位
                            String baseSym = symStr.substring(0, symStr.length() - 4);
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + baseSym + "@GOTPAGE"));
                            assembly.add(
                                    new Directive("\tldr\t" + dst + ", [" + dst + ", " + baseSym + "@GOTPAGEOFF]"));
                        } else if (ent.type().isFunction() && !isDefinedHere(ent)) {
                            // 外部函数：通过GOT取地址
                            assembly.add(new Directive("\tadrp\t" + dst + ", " + symStr + "@GOTPAGE"));
                            assembly.add(new Directive("\tldr\t" + dst + ", [" + dst + ", " + symStr + "@GOTPAGEOFF]"));
                        } else {
                            // 其它：PAGE/PAGEOFF
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

    private void storeToVar(Var v, Register src) {
        Entity ent = v.entity();
        long sz = v.type().size();
        boolean isFloat = ent.type().isFloat();

        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (isFloat) {
                // 浮点数变量，源寄存器应该是浮点寄存器
                if (off >= 0) {
                    assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + off + "]"));
                } else {
                    assembly.add(new Directive("\tstr\t" + src + ", [x29, #-" + (-off) + "]"));
                }
            } else if (sz == 8) {
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
                if (isFloat) {
                    // 浮点数参数
                    if (localOff >= 0) {
                        assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + localOff + "]"));
                    } else {
                        assembly.add(new Directive("\tstr\t" + src + ", [x29, #-" + (-localOff) + "]"));
                    }
                } else {
                    // 整数参数
                    if (localOff >= 0) {
                        assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + localOff + "]"));
                    } else {
                        assembly.add(new Directive("\tstr\t" + src + ", [x29, #-" + (-localOff) + "]"));
                    }
                }
            } else {
                // 如果没有局部变量偏移，使用参数偏移
                if (isFloat) {
                    // 浮点数参数
                    assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + off + "]"));
                } else {
                    // 整数参数
                    assembly.add(new Directive("\tstr\t" + src + ", [x29, #" + off + "]"));
                }
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
            if (isFloat) {
                // 浮点数外部变量
                assembly.add(new Directive("\tstr\t" + src + ", [x0]"));
            } else {
                // 整数外部变量
                assembly.add(new Directive("\tstr\t" + src + ", [x0]"));
            }
        }
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
            long uv = ~(-v);
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
     * 分配整数临时寄存器，如果池为空则处理溢出（支持重复spill）
     */
    private Register allocateTempRegisterWithSpill(TempRegisterAllocationContext context, String label) {
        return allocateTempRegisterWithSpill(context, label, false);
    }

    /**
     * 分配浮点数临时寄存器，如果池为空则处理溢出（支持重复spill）
     */
    private Register allocateFloatTempRegisterWithSpill(TempRegisterAllocationContext context, String label) {
        return allocateTempRegisterWithSpill(context, label, true);
    }

    /**
     * 分配临时寄存器，如果池为空则处理溢出（支持重复spill）
     */
    private Register allocateTempRegisterWithSpill(TempRegisterAllocationContext context, String label,
            boolean isFloat) {
        context.recordAlloc();
        Register reg = registerAllocator.allocateTempRegister(isFloat);
        if (reg == null) {
            // 需要溢出，获取需要溢出的寄存器
            Register spilledReg = registerAllocator.getRegisterToSpill(context, isFloat);

            // 检查这个寄存器是否已经被当前context分配过
            if (context.isRegisterAllocated(spilledReg)) {
                throw new IllegalStateException("Register " + spilledReg.name()
                        + " is already allocated in this context. Source: " + context.source + ", Label: " + label);
            }

            long relativeOffset = registerAllocator.getNextSpillOffset();
            // 临时寄存器溢出使用相对于sp的偏移量，指向栈帧底部
            // 栈帧布局：[sp] -> [局部变量] -> [临时寄存器溢出] -> [寄存器保存] -> [参数]
            long absoluteOffset = spillOffset + relativeOffset; // 相对于sp的偏移量
            // 检查寄存器是否已经被spill过
            int spillDepth = registerAllocator.getSpillDepth(spilledReg);
            System.err.println("allocateTempRegisterWithSpill " + spilledReg.name() + " to " + absoluteOffset
                    + " (spill depth: " + spillDepth + ")" + " source:" + context.source + " label: " + label
                    + " float: " + isFloat);

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
            System.err.println(
                    "allocateTempRegisterWithSpill " + reg.name() + " source: " + context.source + " label:" + label
                            + " float: " + isFloat);
        }

        // 记录这个寄存器被当前context分配
        context.recordRegisterAllocation(reg);
        return reg;
    }

    /**
     * 释放临时寄存器，如果被溢出则恢复（支持重复spill）
     */
    private void releaseTempRegisterWithRestore(Register register, TempRegisterAllocationContext context) {
        // 检查这个寄存器是否被溢出
        context.recordRelease();
        context.recordRegisterRelease(register); // 记录寄存器释放
        Long relativeOffset = registerAllocator.getSpillOffset(register);
        if (relativeOffset != null) {
            // 临时寄存器溢出使用相对于sp的偏移量
            long absoluteOffset = spillOffset + relativeOffset; // 相对于sp的偏移量

            // 获取spill深度信息
            int spillDepth = registerAllocator.getSpillDepth(register);
            String errMessage = "releaseTempRegisterWithRestore " + register.name() + " from " + absoluteOffset
                    + " (spill depth: " + spillDepth + ")" + " source:" + context.source;
            System.err.println(errMessage);

            // 生成恢复代码：从栈上恢复寄存器内容
            // 使用x29作为基址寄存器，偏移量相对于x29
            assembly.add(new Directive("\tldr\t" + register + ", [x29, #-" + absoluteOffset + "]"));
            // 清理溢出状态（弹出栈顶）
            registerAllocator.clearSpill(register);
        } else {
            System.err.println("releaseTempRegisterWithRestore " + register.name() + " source:" + context.source);
            // 释放寄存器
            registerAllocator.releaseTempRegister(register);
        }
    }

    /**
     * 保存caller-saved寄存器到栈上
     */
    private void saveCallerSavedRegisters(Set<Register> usedCallerSaved) {
        // 定义caller-saved寄存器的保存顺序（按对保存，保持16字节对齐）
        Register[][] registerPairs = {
                { Register.X9, Register.X10 },
                { Register.X11, Register.X12 },
                // { Register.X13, Register.X14 },
                // { Register.X15, Register.X16 }
        };

        for (Register[] pair : registerPairs) {
            boolean shouldSavePair = true;
            // for (Register reg : pair) {
            // if (usedCallerSaved.contains(reg)) {
            // shouldSavePair = true;
            // break;
            // }
            // }

            if (shouldSavePair) {
                assembly.add(new Directive("\tstp\t" + pair[0] + ", " + pair[1] + ", [sp, #-16]!"));
            }
        }
    }

    /**
     * 从栈上恢复caller-saved寄存器
     */
    private void restoreCallerSavedRegisters(Set<Register> usedCallerSaved) {
        // 定义caller-saved寄存器的恢复顺序（按对恢复，保持16字节对齐）
        Register[][] registerPairs = {
                // { Register.X15, Register.X16 },
                // { Register.X13, Register.X14 },
                { Register.X11, Register.X12 },
                { Register.X9, Register.X10 }
        };

        for (Register[] pair : registerPairs) {
            boolean shouldRestorePair = true;
            // for (Register reg : pair) {
            // if (usedCallerSaved.contains(reg)) {
            // shouldRestorePair = true;
            // break;
            // }
            // }

            if (shouldRestorePair) {
                assembly.add(new Directive("\tldp\t" + pair[0] + ", " + pair[1] + ", [sp], #16"));
            }
        }
    }

    // ====== RegisterAwareVisitor Implementation ======

    @Override
    public Void visit(Expr expr, Register targetRegister) {
        // 根据表达式类型分发到具体的处理方法
        if (expr instanceof Bin) {
            return visit((Bin) expr, targetRegister);
        } else if (expr instanceof Uni) {
            return visit((Uni) expr, targetRegister);
        } else if (expr instanceof Var) {
            return visit((Var) expr, targetRegister);
        } else if (expr instanceof net.loveruby.cflat.ir.Float) {
            return visit((net.loveruby.cflat.ir.Float) expr, targetRegister);
        } else if (expr instanceof Int) {
            return visit((Int) expr, targetRegister);
        } else if (expr instanceof Str) {
            return visit((Str) expr, targetRegister);
        } else if (expr instanceof Call) {
            return visit((Call) expr, targetRegister);
        } else if (expr instanceof Addr) {
            return visit((Addr) expr, targetRegister);
        } else if (expr instanceof Mem) {
            return visit((Mem) expr, targetRegister);
        } else {
            throw new Error("Unsupported expression type: " + expr.getClass().getSimpleName());
        }
    }

    @Override
    public Void visit(Stmt stmt) {
        // 根据语句类型分发到具体的处理方法
        if (stmt instanceof Assign) {
            return visit((Assign) stmt);
        } else if (stmt instanceof ExprStmt) {
            return visit((ExprStmt) stmt);
        } else if (stmt instanceof CJump) {
            return visit((CJump) stmt);
        } else if (stmt instanceof Jump) {
            return visit((Jump) stmt);
        } else if (stmt instanceof Switch) {
            return visit((Switch) stmt);
        } else if (stmt instanceof LabelStmt) {
            return visit((LabelStmt) stmt);
        } else if (stmt instanceof Return) {
            return visit((Return) stmt);
        } else {
            throw new Error("Unsupported statement type: " + stmt.getClass().getSimpleName());
        }
    }

    // ====== Register-aware visit methods for expressions ======

    public Void visit(Bin e, Register targetRegister) {
        // 根据操作类型和操作数类型选择合适的寄存器
        boolean isFloatOp = isFloatOperation(e);
        if (isFloatOp) {
            handleFloatBinaryOp(e, targetRegister);
        } else {
            handleIntegerBinaryOp(e, targetRegister);
        }
        return null;
    }

    public Void visit(Uni e, Register targetRegister) {
        // 记录传入寄存器的分配，避免寄存器分配重复
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(2, "Uni");
        context.recordRegisterAllocation(targetRegister);

        // 一元操作：先计算操作数到目标寄存器，然后应用操作
        e.expr().accept(this, targetRegister);

        String targetRegName = targetRegister.name();
        switch (e.op()) {
            case UMINUS:
                if (isFloatOperand(e.expr())) {
                    assembly.add(new Directive("\tfneg\t" + targetRegister.name() + ", " + targetRegister.name()));
                } else {
                    assembly.add(new Directive("\tneg\t" + targetRegName + ", " + targetRegName));
                }
                break;
            case BIT_NOT:
                if (e.type().size() == 4) {
                    assembly.add(new Directive(
                            "\tmvn\tw" + targetRegName.substring(1) + ", w" + targetRegName.substring(1)));
                } else {
                    assembly.add(new Directive("\tmvn\t" + targetRegName + ", " + targetRegName));
                }
                break;
            case NOT:
                assembly.add(new Directive("\tcmp\t" + targetRegName + ", #0"));
                assembly.add(new Directive("\tcset\t" + targetRegName + ", eq"));
                break;
            case S_CAST:
                break;
            case U_CAST:
                if (e.expr().type().size() == 4 && e.type().size() == 8) {
                    assembly.add(new Directive("\tuxtw\t" + targetRegName + ", w" + targetRegName.substring(1)));
                }
                break;
            default:
                throw new Error("Unsupported unary operation: " + e.op());
        }
        return null;
    }

    public Void visit(Var e, Register targetRegister) {
        // 记录传入寄存器的分配，避免寄存器分配重复
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(2, "Var");
        context.recordRegisterAllocation(targetRegister);

        // 变量加载到指定寄存器
        String targetRegName = targetRegister.name();
        Entity entity = e.entity();

        if (entity.isConstant()) {
            // 常量直接加载
            if (entity.type().isFloat()) {
                // 浮点常量需要特殊处理
                assembly.add(new Directive("\tadrp\t" + targetRegName + ", " + entity.name() + "@PAGE"));
                assembly.add(new Directive(
                        "\tadd\t" + targetRegName + ", " + targetRegName + ", " + entity.name() + "@PAGEOFF"));
                assembly.add(new Directive("\tldr\t" + targetRegister.name() + ", [" + targetRegName + "]"));
            } else {
                assembly.add(new Directive("\tadrp\t" + targetRegName + ", " + entity.name() + "@PAGE"));
                assembly.add(new Directive(
                        "\tadd\t" + targetRegName + ", " + targetRegName + ", " + entity.name() + "@PAGEOFF"));
                assembly.add(new Directive("\tldr\t" + targetRegName + ", [" + targetRegName + "]"));
            }
        } else {
            // 变量从栈或寄存器加载
            net.loveruby.cflat.asm.Register reg = registerMap.get(entity);

            if (reg != null) {
                // 变量在寄存器中，直接移动到目标寄存器
                System.err.println("Loading " + entity.name() + " from register " + reg);
                if (entity.type().isFloat() && targetRegister.name().startsWith("d")) {
                    // 浮点数寄存器之间使用fmov指令
                    assembly.add(new Directive("\tfmov\t" + targetRegName + ", " + reg));
                } else if (entity.type().isFloat() && targetRegister.name().startsWith("x")) {
                    // 浮点数寄存器移动到整数寄存器，需要特殊处理
                    // 暂时跳过，因为printf需要的是浮点数值
                    assembly.add(new Directive("\tmov\t" + targetRegName + ", " + reg));
                } else if (!entity.type().isFloat() && targetRegister.name().startsWith("d")) {
                    // 整数寄存器移动到浮点数寄存器，需要特殊处理
                    assembly.add(new Directive("\tmov\t" + targetRegName + ", " + reg));
                } else {
                    // 整数寄存器之间使用mov指令
                    assembly.add(new Directive("\tmov\t" + targetRegName + ", " + reg));
                }
            } else if (spillOffsets.containsKey(entity)) {
                // 变量溢出到栈上，从栈加载 - 根据变量类型使用正确的加载指令
                long offset = spillOffsets.get(entity);
                System.err.println("Loading " + entity.name() + " from spill offset " + offset);

                // 根据变量类型使用正确的加载指令
                if (entity.type().isFloat()) {
                    // 浮点数变量使用浮点数加载指令
                    assembly.add(new Directive("\tldr\t" + targetRegister.name() + ", [x29, #" + offset + "]"));
                } else {
                    // 整数变量根据大小使用不同的加载指令
                    long sz = e.type().size();
                    if (sz == 8) {
                        // 64位变量：直接使用64位加载指令
                        assembly.add(new Directive("\tldr\t" + targetRegName + ", [x29, #" + offset + "]"));
                    } else if (sz == 4) {
                        // 32位变量：使用32位加载指令，然后有符号扩展到64位
                        assembly.add(
                                new Directive("\tldr\tw" + targetRegName.substring(1) + ", [x29, #" + offset + "]"));
                    } else if (sz == 2) {
                        // 16位变量：使用16位加载指令，然后有符号扩展到64位
                        assembly.add(
                                new Directive("\tldrh\tw" + targetRegName.substring(1) + ", [x29, #" + offset + "]"));
                        assembly.add(new Directive("\tsxth\t" + targetRegName + ", w" + targetRegName.substring(1)));
                    } else if (sz == 1) {
                        // 8位变量：使用8位加载指令，然后有符号扩展到64位
                        assembly.add(
                                new Directive("\tldrb\tw" + targetRegName.substring(1) + ", [x29, #" + offset + "]"));
                        assembly.add(new Directive("\tsxtb\t" + targetRegName + ", w" + targetRegName.substring(1)));
                    } else {
                        errorHandler.error("unsupported load size: " + sz);
                    }
                }
            } else {
                // 使用原有的栈访问方法
                System.err.println("Loading " + entity.name() + " from stack");
                loadFromVar(e, targetRegName);
            }
        }
        return null;
    }

    public Void visit(Int e, Register targetRegister) {
        // 记录传入寄存器的分配，避免寄存器分配重复
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(2, "Int");
        context.recordRegisterAllocation(targetRegister);

        // 检查目标寄存器是否是浮点寄存器
        String targetRegName = targetRegister.name();
        boolean isFloatTarget = targetRegName.startsWith("d");

        // 检查AST类型来确定是否为浮点数
        boolean isFloatValue = e.astType() != null && e.astType().isFloat();

        if (isFloatValue || isFloatTarget) {
            // 浮点数处理
            if (isFloatValue) {
                // 这是一个浮点数字面量
                double doubleValue = Double.longBitsToDouble(e.value());
                if (doubleValue == 0.0) {
                    assembly.add(new Directive("\tfmov\t" + targetRegName + ", #0.0"));
                } else if (doubleValue == 1.0) {
                    assembly.add(new Directive("\tfmov\t" + targetRegName + ", #1.0"));
                } else if (doubleValue == 2.0) {
                    assembly.add(new Directive("\tfmov\t" + targetRegName + ", #1.0"));
                } else {
                    // 对于复杂的浮点数，需要先加载到整数寄存器，然后移动到浮点寄存器
                    // 这是ARM64的限制
                    materializeImmediate64("x0", e.value(), false);
                    assembly.add(new Directive("\tfmov\t" + targetRegName + ", x0"));
                }
            } else {
                // 目标寄存器是浮点寄存器，但值不是浮点数
                // 需要将整数值转换为浮点数
                materializeImmediate64("x0", e.value(), false);
                assembly.add(new Directive("\tfmov\t" + targetRegName + ", x0"));
            }
        } else {
            // 整数寄存器，使用原来的逻辑
            long v = e.value();
            if (e.type().size() == 4) {
                // 32位整数：直接使用32位寄存器，不进行扩展
                assembly.add(new Directive("\tmov\tw" + targetRegName.substring(1) + ", #" + (v & 0xFFFFFFFFL)));
            } else {
                materializeImmediate64(targetRegName, v, false);
            }
        }
        return null;
    }

    public Void visit(net.loveruby.cflat.ir.Float e, Register targetRegister) {
        // 记录传入寄存器的分配，避免寄存器分配重复
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(2, "Float64");
        context.recordRegisterAllocation(targetRegister);

        // 浮点数常量加载到指定寄存器
        String targetRegName = targetRegister.name();
        double value = e.value();

        if (targetRegName.startsWith("d")) {
            // 目标寄存器是浮点寄存器
            if (value == 0.0) {
                assembly.add(new Directive("\tfmov\t" + targetRegName + ", #0.0"));
            } else if (value == 1.0) {
                assembly.add(new Directive("\tfmov\t" + targetRegName + ", #1.0"));
            } else if (value == 2.0) {
                assembly.add(new Directive("\tfmov\t" + targetRegName + ", #2.0"));
            } else {
                // 对于复杂的浮点数，需要先加载到整数寄存器，然后移动到浮点寄存器
                // 这是ARM64的限制
                long bits = Double.doubleToLongBits(value);
                materializeImmediate64("x0", bits, false);
                assembly.add(new Directive("\tfmov\t" + targetRegName + ", x0"));
            }
        } else {
            // 目标寄存器是整数寄存器，需要将浮点数作为位模式加载
            long bits = Double.doubleToLongBits(value);
            materializeImmediate64(targetRegName, bits, false);
        }
        return null;
    }

    public Void visit(Str e, Register targetRegister) {
        // 记录传入寄存器的分配，避免寄存器分配重复
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(2, "Str");
        context.recordRegisterAllocation(targetRegister);

        // 字符串地址加载到指定寄存器
        String targetRegName = targetRegister.name();
        assembly.add(new Directive("\tadrp\t" + targetRegName + ", " + e.symbol().name() + "@PAGE"));
        assembly.add(new Directive(
                "\tadd\t" + targetRegName + ", " + targetRegName + ", " + e.symbol().name() + "@PAGEOFF"));
        return null;
    }

    public Void visit(Call e, Register targetRegister) {
        // 记录传入寄存器的分配，避免寄存器分配重复
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(registerAllocator.getSpillSlotCount(),
                "Call");
        context.recordRegisterAllocation(targetRegister);

        // 函数调用，结果放在指定寄存器
        String targetRegName = targetRegister.name();

        boolean isVar = e.isStaticCall() &&
                e.function().type().getFunctionType().isVararg();
        List<Expr> args = e.args();
        int total = args.size();
        int fixed = 0;
        if (e.isStaticCall()) {
            fixed = e.function().type().getFunctionType().paramTypes().size();
        }

        // 优化：只保存实际使用到的caller-saved寄存器
        boolean shouldSaveCallerRegister = !e.isStaticCall() || !e.function().name().equals("alloca");
        Set<Register> usedCallerSaved = new HashSet<>();
        if (shouldSaveCallerRegister) {
            // 获取在函数调用点实际活跃的caller-saved寄存器
            usedCallerSaved = registerAllocator.getLiveCallerSavedRegisters(currentStmt);
            System.err.println("Live caller-saved registers at call site: "
                    + usedCallerSaved.stream().map(Register::name).collect(Collectors.joining(", ")));

            // 按寄存器对保存，保持16字节对齐
            saveCallerSavedRegisters(usedCallerSaved);
        }

        int stackArgs = Math.max(0, total - ARG_REGS.length);
        int shadow = isVar ? 8 * (total - fixed) : 0;
        int temp = total * 8;
        int block = shadow + stackArgs * 8 + temp;
        block = (block + 15) & ~15;

        if (block > 0)
            assembly.add(new Directive("\tsub\tsp, sp, #" + block));

        int tempBase = shadow + stackArgs * 8;

        // Pass1: L->R - 处理浮点数和整数参数
        // 整数参数过多，需要存储到栈上
        // 其实之前的two pass的实现是有问题的，如果命名参数超过8个的情况下，fixed参数的摆放问题应该重新考虑
        Register stackTemp = allocateTempRegisterWithSpill(context, "Call");
        Register stackTempFloat = allocateFloatTempRegisterWithSpill(context, "Call");
        int floatIndex = -1;
        int intIndex = -1;
        for (int i = 0; i < total; ++i) {
            Expr arg = args.get(i);
            if (isFloatOperand(arg)) {
                floatIndex++;
                // 浮点数参数：直接加载到浮点寄存器
                if (floatIndex < FLOAT_ARG_REGS.length) {
                    arg.accept(this, FLOAT_ARG_REGS[floatIndex]);
                } else {
                    // 浮点数参数过多，需要存储到栈上
                    long off = shadow + (long) (i - ARG_REGS.length) * 8;
                    arg.accept(this, stackTempFloat); // 临时使用d0
                    assembly.add(new Directive("\tstr\t" + stackTempFloat.name() + ", [sp, #" + (off) + "]"));

                }
                String dst;
                if (i < ARG_REGS.length) {
                    dst = FLOAT_ARG_REGS[floatIndex].toString();
                } else {
                    dst = stackTempFloat.toString();
                }
                if (isVar && i >= fixed) {
                    long sh = (i - fixed) * 8L;
                    assembly.add(new Directive("\tstr\t" + dst + ", [sp, #" + sh + "]"));
                }
            } else {
                // 整数参数：加载到整数寄存器
                intIndex++;
                if (i < ARG_REGS.length) {
                    arg.accept(this, ARG_REGS[intIndex]);
                } else {
                    arg.accept(this, stackTemp); // 临时使用x0
                    long off = shadow + (long) (i - ARG_REGS.length) * 8;
                    assembly.add(new Directive("\tstr\t" + stackTemp.name() + ", [sp, #" + (off) + "]"));
                }
                String dst;
                if (intIndex < ARG_REGS.length) {
                    dst = ARG_REGS[intIndex].toString();
                } else {
                    dst = stackTemp.toString();
                }
                if (isVar && i >= fixed) {
                    long sh = (i - fixed) * 8L;
                    assembly.add(new Directive("\tstr\t" + dst + ", [sp, #" + sh + "]"));
                }
            }
        }
        releaseTempRegisterWithRestore(stackTemp, context);
        releaseTempRegisterWithRestore(stackTempFloat, context);
        if (e.isStaticCall()) {
            // 对于变长参数函数，我们需要特殊处理va_init调用
            if (e.function().name().equals("va_init")) {
                // va_init期望接收当前栈指针的值
                assembly.add(new Directive("\tmov\tx0, sp"));
            }
            assembly.add(new Directive("\tbl\t" + e.function().callingSymbol().toSource()));
        } else {
            // 对于间接函数调用，需要将函数地址加载到不同的寄存器，避免覆盖x0中的参数
            Register tempRegister = allocateTempRegisterWithSpill(context, "tmp pointer");
            e.expr().accept(this, tempRegister); // callee -> x0
            assembly.add(new Directive("\tblr\t" + tempRegister)); // 使用临时寄存器调用函数
            releaseTempRegisterWithRestore(tempRegister, context);
        }

        if (block > 0)
            assembly.add(new Directive("\tadd\tsp, sp, #" + block));
        if (shouldSaveCallerRegister) {
            // 恢复caller-saved寄存器，使用ldp保持16字节对齐
            restoreCallerSavedRegisters(usedCallerSaved);
        }

        // 如果目标寄存器不是x0，需要移动结果
        if (!targetRegName.equals("x0")) {
            assembly.add(new Directive("\tmov\t" + targetRegName + ", x0"));
        }

        return null;
    }

    public Void visit(Addr e, Register targetRegister) {
        // 记录传入寄存器的分配，避免寄存器分配重复
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(2, "Addr");
        context.recordRegisterAllocation(targetRegister);

        // 取地址操作
        String targetRegName = targetRegister.name();
        addrOfEntityInto(e.entity(), targetRegName);
        return null;
    }

    public Void visit(Mem e, Register targetRegister) {
        // 记录传入寄存器的分配，避免寄存器分配重复
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(registerAllocator.getSpillSlotCount(),
                "Mem");
        context.recordRegisterAllocation(targetRegister);

        // 内存访问
        String targetRegName = targetRegister.name();
        String target32Name = targetRegister.bit32Name();

        // 计算地址到临时寄存器，避免覆盖目标寄存器
        Register temp = allocateTempRegisterWithSpill(context, "mem");
        evalAddressInto(e.expr(), temp);
        String tempRef = "[" + temp.name() + "]";
        long sz = e.type().size();

        if (sz == 1) {
            // 读 1 字节并做有符号扩展成 64 位
            assembly.add(new Directive("\tldrb\t" + target32Name + ", " + tempRef));
            assembly.add(new Directive("\tsxtb\t" + targetRegName + ", " + target32Name));
        } else if (sz == 2) {
            assembly.add(new Directive("\tldrh\t" + target32Name + ", " + tempRef));
            assembly.add(new Directive("\tsxth\t" + targetRegName + ", " + target32Name));
        } else if (sz == 4) {
            assembly.add(new Directive("\tldr\t" + target32Name + ", " + tempRef));
        } else if (sz == 8) {
            assembly.add(new Directive("\tldr\t" + targetRegName + ", " + tempRef));
        } else {
            errorHandler.error("unsupported load size: " + sz);
        }
        releaseTempRegisterWithRestore(temp, context);
        context.checkState();
        return null;
    }

    // ====== Helper methods for register-aware operations ======

    private boolean isFloatOperation(Bin e) {
        return switch (e.op()) {
            case ADD, SUB, MUL, S_DIV -> isFloatOperand(e.left()) || isFloatOperand(e.right());
            default -> false;
        };
    }

    private boolean isFloatOperand(Expr expr) {
        if (expr instanceof Var) {
            Var var = (Var) expr;
            return var.entity().type().isFloat();
        }
        return false;
    }

    private void handleFloatBinaryOp(Bin e, Register targetReg) {
        // 为左操作数分配浮点数寄存器
        TempRegisterAllocationContext leftContext = new TempRegisterAllocationContext(2, "bin_left");
        leftContext.recordRegisterAllocation(targetReg);
        Register leftReg = allocateFloatTempRegisterWithSpill(leftContext, "bin_left");
        e.left().accept(this, leftReg);

        // 为右操作数分配浮点数寄存器
        TempRegisterAllocationContext rightContext = new TempRegisterAllocationContext(2, "bin_right");
        Register rightReg = allocateFloatTempRegisterWithSpill(rightContext, "bin_right");
        e.right().accept(this, rightReg);

        // 执行浮点运算
        switch (e.op()) {
            case ADD:
                assembly.add(
                        new Directive("\tfadd\t" + targetReg.name() + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case SUB:
                assembly.add(
                        new Directive("\tfsub\t" + targetReg.name() + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case MUL:
                assembly.add(
                        new Directive("\tfmul\t" + targetReg.name() + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case S_DIV:
                assembly.add(
                        new Directive("\tfdiv\t" + targetReg.name() + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            default:
                throw new Error("Unsupported float operation: " + e.op());
        }

        // 释放临时寄存器
        releaseTempRegisterWithRestore(rightReg, rightContext);
        releaseTempRegisterWithRestore(leftReg, leftContext);
    }

    private void handleIntegerBinaryOp(Bin e, Register targetReg) {
        System.err.println("handleIntegerBinaryOp " + e);
        // 为左操作数分配寄存器
        TempRegisterAllocationContext context = new TempRegisterAllocationContext(2, "bin_left");
        context.recordRegisterAllocation(targetReg);
        Register leftReg = allocateTempRegisterWithSpill(context, "bin_left");
        e.left().accept(this, leftReg);

        // 为右操作数分配寄存器
        Register rightReg = allocateTempRegisterWithSpill(context, "bin_right");
        e.right().accept(this, rightReg);

        // 执行整数运算
        switch (e.op()) {
            case ADD:
                assembly.add(new Directive("\tadd\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case SUB:
                assembly.add(new Directive("\tsub\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case MUL:
                assembly.add(new Directive("\tmul\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case S_DIV:
                assembly.add(new Directive("\tsdiv\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case U_DIV:
                assembly.add(new Directive("\tudiv\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case S_MOD:
                assembly.add(new Directive("\tsdiv\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tmul\t" + targetReg + ", " + targetReg + ", " + rightReg.name()));
                assembly.add(new Directive("\tsub\t" + targetReg + ", " + leftReg.name() + ", " + targetReg));
                break;
            case U_MOD:
                assembly.add(new Directive("\tudiv\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tmul\t" + targetReg + ", " + targetReg + ", " + rightReg.name()));
                assembly.add(new Directive("\tsub\t" + targetReg + ", " + leftReg.name() + ", " + targetReg));
                break;
            case BIT_AND:
                assembly.add(new Directive("\tand\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case BIT_OR:
                assembly.add(new Directive("\torr\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case BIT_XOR:
                assembly.add(new Directive("\teor\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case BIT_LSHIFT:
                assembly.add(new Directive("\tlsl\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case BIT_RSHIFT:
                assembly.add(new Directive("\tlsr\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case ARITH_RSHIFT:
                assembly.add(new Directive("\tasr\t" + targetReg + ", " + leftReg.name() + ", " + rightReg.name()));
                break;
            case EQ:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", eq"));
                break;
            case NEQ:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", ne"));
                break;
            case S_LT:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", lt"));
                break;
            case S_LTEQ:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", le"));
                break;
            case S_GT:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", gt"));
                break;
            case S_GTEQ:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", ge"));
                break;
            case U_LT:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", lo"));
                break;
            case U_LTEQ:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", ls"));
                break;
            case U_GT:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", hi"));
                break;
            case U_GTEQ:
                assembly.add(new Directive("\tcmp\t" + leftReg.name() + ", " + rightReg.name()));
                assembly.add(new Directive("\tcset\t" + targetReg + ", hs"));
                break;
            default:
                throw new Error("Unsupported integer operation: " + e.op());
        }

        // 释放临时寄存器
        releaseTempRegisterWithRestore(rightReg, context);
        releaseTempRegisterWithRestore(leftReg, context);
    }

    private void loadFromVar(Var v, String targetRegister) {
        Entity ent = v.entity();
        long sz = v.type().size();
        boolean isFloat = ent.type().isFloat();

        if (registerAllocator.isInRegister(ent)) {
            Register register = registerAllocator.getRegister(ent);
            if (isFloat && targetRegister.startsWith("d")) {
                // 浮点数寄存器之间直接移动
                assembly.add(new Directive("\tfmov\t" + targetRegister + ", " + register.name()));
                return;
            } else if (isFloat && targetRegister.startsWith("x")) {
                // 浮点数寄存器移动到整数寄存器，需要特殊处理
                // 在ARM64中，不能直接用mov在浮点寄存器和整数寄存器之间移动
                // 需要先将浮点数存储到栈上，然后作为位模式加载
                assembly.add(new Directive("\tstr\t" + register.name() + ", [sp, #-16]!"));
                assembly.add(new Directive("\tldr\t" + targetRegister + ", [sp], #16"));
                return;
            } else if (!isFloat && targetRegister.startsWith("d")) {
                // 整数寄存器移动到浮点数寄存器，需要特殊处理
                // 在ARM64中，不能直接用mov在整数寄存器和浮点寄存器之间移动
                // 需要先将整数存储到栈上，然后作为位模式加载
                assembly.add(new Directive("\tstr\t" + register.name() + ", [sp, #-16]!"));
                assembly.add(new Directive("\tldr\t" + targetRegister + ", [sp], #16"));
                return;
            } else {
                // 整数寄存器之间移动
                assembly.add(new Directive("\tmov\t" + targetRegister + ", " + register.name()));
                return;
            }
        }

        // 对于不在寄存器中的变量，根据类型选择加载方式
        if (localVarOffsets.containsKey(ent)) {
            long off = localVarOffsets.get(ent);
            if (isFloat) {
                if (targetRegister.startsWith("d")) {
                    // 浮点数直接加载到浮点寄存器
                    assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + off + "]"));
                } else {
                    // 浮点数加载到整数寄存器（作为位模式）
                    assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + off + "]"));
                }
            } else if (sz == 8) {
                assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + off + "]"));
            } else if (sz == 4) {
                assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + off + "]"));
            } else if (sz == 2) {
                assembly.add(new Directive("\tldrh\t" + targetRegister + ", [x29, #" + off + "]"));
                if (targetRegister.startsWith("x")) {
                    assembly.add(new Directive("\tsxth\t" + targetRegister + ", " + targetRegister));
                }
            } else if (sz == 1) {
                assembly.add(new Directive("\tldrb\t" + targetRegister + ", [x29, #" + off + "]"));
                if (targetRegister.startsWith("x")) {
                    assembly.add(new Directive("\tsxtb\t" + targetRegister + ", " + targetRegister));
                }
            } else {
                errorHandler.error("unsupported load size: " + sz);
            }
        } else if (paramOffsets.containsKey(ent)) {
            long off = paramOffsets.get(ent);
            // 对于参数，优先从局部变量位置加载（如果存在）
            if (localVarOffsets.containsKey(ent)) {
                long localOff = localVarOffsets.get(ent);
                if (isFloat) {
                    if (targetRegister.startsWith("d")) {
                        // 浮点数直接加载到浮点寄存器
                        assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + localOff + "]"));
                    } else {
                        // 浮点数加载到整数寄存器（作为位模式）
                        assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + localOff + "]"));
                    }
                } else if (sz == 8) {
                    assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + localOff + "]"));
                } else if (sz == 4) {
                    assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + localOff + "]"));
                } else if (sz == 2) {
                    assembly.add(new Directive("\tldrh\t" + targetRegister + ", [x29, #" + localOff + "]"));
                    if (targetRegister.startsWith("x")) {
                        assembly.add(new Directive("\tsxth\t" + targetRegister + ", " + targetRegister));
                    }
                } else if (sz == 1) {
                    assembly.add(new Directive("\tldrb\t" + targetRegister + ", [x29, #" + localOff + "]"));
                    if (targetRegister.startsWith("x")) {
                        assembly.add(new Directive("\tsxtb\t" + targetRegister + ", " + targetRegister));
                    }
                } else {
                    errorHandler.error("unsupported load size: " + sz);
                }
            } else {
                // 在 ARM64 中，前 8 个参数通过寄存器传递
                // 参数在栈上，从栈加载
                if (isFloat) {
                    if (targetRegister.startsWith("d")) {
                        // 浮点数直接加载到浮点寄存器
                        assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + off + "]"));
                    } else {
                        // 浮点数加载到整数寄存器（作为位模式）
                        assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + off + "]"));
                    }
                } else if (sz == 8) {
                    assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + off + "]"));
                } else if (sz == 4) {
                    assembly.add(new Directive("\tldr\t" + targetRegister + ", [x29, #" + off + "]"));
                } else if (sz == 2) {
                    assembly.add(new Directive("\tldrh\t" + targetRegister + ", [x29, #" + off + "]"));
                    if (targetRegister.startsWith("x")) {
                        assembly.add(new Directive("\tsxth\t" + targetRegister + ", " + targetRegister));
                    }
                } else if (sz == 1) {
                    assembly.add(new Directive("\tldrb\t" + targetRegister + ", [x29, #" + off + "]"));
                    if (targetRegister.startsWith("x")) {
                        assembly.add(new Directive("\tsxtb\t" + targetRegister + ", " + targetRegister));
                    }
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

            // 从内存加载值
            if (isFloat) {
                if (targetRegister.startsWith("d")) {
                    // 浮点数直接加载到浮点寄存器
                    assembly.add(new Directive("\tldr\t" + targetRegister + ", [x0]"));
                } else {
                    // 浮点数加载到整数寄存器（作为位模式）
                    assembly.add(new Directive("\tldr\t" + targetRegister + ", [x0]"));
                }
            } else if (sz == 8) {
                assembly.add(new Directive("\tldr\t" + targetRegister + ", [x0]"));
            } else if (sz == 4) {
                assembly.add(new Directive("\tldr\t" + targetRegister + ", [x0]"));
            } else if (sz == 2) {
                assembly.add(new Directive("\tldrh\t" + targetRegister + ", [x0]"));
                if (targetRegister.startsWith("x")) {
                    assembly.add(new Directive("\tsxth\t" + targetRegister + ", " + targetRegister));
                }
            } else if (sz == 1) {
                assembly.add(new Directive("\tldrb\t" + targetRegister + ", [x0]"));
                if (targetRegister.startsWith("x")) {
                    assembly.add(new Directive("\tsxtb\t" + targetRegister + ", " + targetRegister));
                }
            } else {
                errorHandler.error("unsupported load size: " + sz);
            }
        }
    }

    /**
     * 根据寄存器名称获取Register对象
     */
    private Register getRegisterByName(String name) {
        return switch (name) {
            case "x0" -> Register.X0;
            case "x1" -> Register.X1;
            case "x2" -> Register.X2;
            case "x3" -> Register.X3;
            case "x4" -> Register.X4;
            case "x5" -> Register.X5;
            case "x6" -> Register.X6;
            case "x7" -> Register.X7;
            case "x8" -> Register.X8;
            case "x9" -> Register.X9;
            case "x10" -> Register.X10;
            case "x11" -> Register.X11;
            case "x12" -> Register.X12;
            case "x13" -> Register.X13;
            case "x14" -> Register.X14;
            case "x15" -> Register.X15;
            case "x16" -> Register.X16;
            case "x17" -> Register.X17;
            case "x18" -> Register.X18;
            case "x19" -> Register.X19;
            case "x20" -> Register.X20;
            case "x21" -> Register.X21;
            case "x22" -> Register.X22;
            case "x23" -> Register.X23;
            case "x24" -> Register.X24;
            case "x25" -> Register.X25;
            case "x26" -> Register.X26;
            case "x27" -> Register.X27;
            case "x28" -> Register.X28;
            case "x29" -> Register.X29;
            case "x30" -> Register.X30;
            // 浮点数寄存器
            case "d0" -> Register.D0;
            case "d1" -> Register.D1;
            case "d2" -> Register.D2;
            case "d3" -> Register.D3;
            case "d4" -> Register.D4;
            case "d5" -> Register.D5;
            case "d6" -> Register.D6;
            case "d7" -> Register.D7;
            case "d8" -> Register.D8;
            case "d9" -> Register.D9;
            case "d10" -> Register.D10;
            case "d11" -> Register.D11;
            case "d12" -> Register.D12;
            case "d13" -> Register.D13;
            case "d14" -> Register.D14;
            case "d15" -> Register.D15;
            default -> throw new Error("Unknown register name: " + name);
        };
    }

}
