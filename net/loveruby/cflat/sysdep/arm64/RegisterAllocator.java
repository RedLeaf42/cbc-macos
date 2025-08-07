package net.loveruby.cflat.sysdep.arm64;

import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.ir.*;

import java.util.*;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * 简化的线性寄存器分配器
 * 基于statement级别的活跃变量分析
 */
public class RegisterAllocator {

    // 寄存器池
    private static final Register[] CALLEE_SAVED = {
            net.loveruby.cflat.sysdep.arm64.Register.X19,
            net.loveruby.cflat.sysdep.arm64.Register.X20,
            net.loveruby.cflat.sysdep.arm64.Register.X21,
            net.loveruby.cflat.sysdep.arm64.Register.X22,
            net.loveruby.cflat.sysdep.arm64.Register.X23,
            net.loveruby.cflat.sysdep.arm64.Register.X24,
            net.loveruby.cflat.sysdep.arm64.Register.X25,
            net.loveruby.cflat.sysdep.arm64.Register.X26
    };

    private static final Register[] CALLER_SAVED = {
            net.loveruby.cflat.sysdep.arm64.Register.X9,
            net.loveruby.cflat.sysdep.arm64.Register.X10,
            net.loveruby.cflat.sysdep.arm64.Register.X11,
            // net.loveruby.cflat.sysdep.arm64.Register.X12,
            // net.loveruby.cflat.sysdep.arm64.Register.X13,
            net.loveruby.cflat.sysdep.arm64.Register.X14,
            net.loveruby.cflat.sysdep.arm64.Register.X15
    };

    // 当前可用的寄存器
    private Set<net.loveruby.cflat.sysdep.arm64.Register> availableCalleeSaved = new LinkedHashSet<>(
            Arrays.asList(CALLEE_SAVED));
    private Set<Register> availableCallerSaved = new LinkedHashSet<>(Arrays.asList(CALLER_SAVED));
    private Map<DefinedVariable, LocalScope> variableLocalScopeMap = new HashMap<>();

    // 变量到寄存器的映射
    private final Map<Entity, net.loveruby.cflat.sysdep.arm64.Register> registerMap = new LinkedHashMap<>();

    // 溢出到栈上的变量,注意，这里记录的是有名变量的溢出位置，目前的实现就是对应变量栈的位置
    private Map<Entity, Long> spillOffsets = new HashMap<>();

    // 为了实现上的简便，每一个slot的大小是8字节
    private int spillSlotCount = 0;

    // 活跃变量分析结果
    private final Map<Stmt, Set<Entity>> liveBefore = new HashMap<>();
    private final Map<Stmt, Set<Entity>> liveAfter = new HashMap<>();
    private final Set<Entity> needsAddress = new LinkedHashSet<>();

    // 变量生命周期
    private final Map<Entity, LifeRange> lifeRanges = new HashMap<>();
    /* 设计这个的目的是为了将寄存器分配逻辑完全放到CodeGenerator中，CodeGenerator不再保留任何硬编码代码 */
    private final Register[] totalTempRegisters = {
             Register.X12,
            Register.X13,
            Register.X16
    };
    private final Set<Register> tempAvaiableRegisterList = new LinkedHashSet<>(Arrays.asList(
            totalTempRegisters));

    // 跟踪已分配的临时寄存器（正在使用中）
    private final Set<Register> allocatedTempRegisters = new LinkedHashSet<>();

    // 跟踪临时寄存器的溢出状态
    private final Map<Register, Long> tempSpillOffsets = new HashMap<>();
    private long nextTempSpillOffset = 0; // 从0开始，每次+8，相对于spillOffset

    // 支持重复spill的栈管理系统
    private final Map<Register, Stack<Long>> registerSpillStack = new HashMap<>();
    private long nextSpillOffset = -8L; // 从-8开始，每次-8，用于变量spill

    public int getSpillSlotCount() {
        return spillSlotCount;
    }

    /**
     * 分配临时寄存器，如果池为空则返回null表示需要溢出
     */
    public Register allocateTempRegister() {
        if (tempAvaiableRegisterList.isEmpty()) {
            // 返回null表示需要溢出
            return null;
        }
        Register item = tempAvaiableRegisterList.iterator().next();
        tempAvaiableRegisterList.remove(item);
        allocatedTempRegisters.add(item); // 记录已分配
        return item;
    }

    /**
     * 获取需要溢出的寄存器（最老的已分配寄存器）
     */
    public Register getRegisterToSpill() {
        if (allocatedTempRegisters.isEmpty()) {
            throw new IllegalStateException("no allocated temp register to spill");
        }
        return allocatedTempRegisters.iterator().next();
    }

    /**
     * 获取下一个溢出偏移量
     */
    public long getNextSpillOffset() {
        long offset = nextTempSpillOffset;
        nextTempSpillOffset += 8;
        return offset;
    }

    /**
     * 记录寄存器溢出状态（支持重复spill）
     */
    public void recordSpill(Register reg, long offset) {
        // 将偏移量压入该寄存器的spill栈
        registerSpillStack.computeIfAbsent(reg, k -> new Stack<>()).push(offset);
        tempSpillOffsets.put(reg, offset);
        System.err.println(
                "Recorded spill for " + reg.name() + " at offset " + offset + " (depth: " + getSpillDepth(reg) + ")");
    }

    /**
     * 获取寄存器的溢出偏移量（最新的）
     */
    public Long getSpillOffset(Register reg) {
        return tempSpillOffsets.get(reg);
    }

    /**
     * 清理寄存器的溢出状态（弹出栈顶）
     */
    public void clearSpill(Register reg) {
        Long poppedOffset = popRegisterSpillStack(reg);
        if (poppedOffset != null) {
            // 更新最新的spill偏移量
            Stack<Long> stack = registerSpillStack.get(reg);
            if (stack != null && !stack.isEmpty()) {
                tempSpillOffsets.put(reg, stack.peek());
            } else {
                tempSpillOffsets.remove(reg);
            }
            System.err.println("Cleared spill for " + reg.name() + " (popped offset: " + poppedOffset + ")");
        }
    }

    public void releaseTempRegister(Register register) {
        boolean valid = false;
        for (Register totalTempRegister : totalTempRegisters) {
            if (totalTempRegister.name().equals(register.name())) {
                valid = true;
                break;
            }
        }
        if (valid && !hasSpillHistory(register)) {
            System.err.println("releaseTempRegister item at " + register.name());
            allocatedTempRegisters.remove(register); // 从已分配列表中移除
            tempAvaiableRegisterList.add(register);
        } else if (valid && hasSpillHistory(register)) {
            System.err.println("releaseTempRegister skipped for " + register.name() + " (has spill history)");
        }
    }

    /**
     * 为函数分配寄存器
     */
    public void allocateRegisters(DefinedFunction func,
            Map<Entity, Long> localVarOffsets,
            Map<Entity, Long> paramOffsets,
            List<Variable> globalVariables,
            LocalScope scope) {
        variableLocalScopeMap.clear();
        registerMap.clear();
        spillOffsets.clear();
        tempAvaiableRegisterList.clear();
        tempAvaiableRegisterList.addAll(Arrays.asList(totalTempRegisters));
        spillSlotCount = 0;
        List<Stmt> statements = func.ir();
        int size = estimateSpillOffset(func);
        spillSlotCount = size;
        System.err.println("allocateRegisters estimateSpillOffset: " + size);
        // 0. 标记始终从内存中获取值的变量
        collectAddressTakenVars(statements);
        collectVariableToScopeMap(scope);

        // 1. 进行活跃变量分析
        analyzeLiveVariables(statements);

        // 2. 构建生命周期
        buildLifeRanges(statements);

        // 3. 分配寄存器
        allocateRegisters(localVarOffsets, paramOffsets, func.parameters(), globalVariables);

        // 4. 生成函数序言（保存callee-saved寄存器）
        generatePrologue(func);

        // 调试信息
        System.err.println("Register allocation for function: " + func.name());
        System.err.println("Local variables: " + func.localVariables().size());
        System.err.println("Parameters: " + func.parameters().size());
        System.err.println("Allocated registers: " + registerMap.size());
        registerMap.forEach((entity, register) -> {
            System.err.println("register: [ " + entity.name() + "], " + register);
        });
        System.err.println("Spilled variables: " + spillOffsets.size());
    }

    private void collectVariableToScopeMap(LocalScope scope) {
        scope.allLocalVariables().forEach(definedVariable -> variableLocalScopeMap.put(definedVariable, scope));
        scope.children().forEach(this::collectVariableToScopeMap);
    }

    private void collectAddressTakenVars(List<Stmt> statements) {
        System.err.println("collectAddressTakenVars start");
        needsAddress.clear();
        for (Stmt stmt : statements) {
            stmt.accept(new DefaultIRVisitor<Void, Void>() {
                @Override
                public Void visit(Addr e) {
                    System.err.println("Address entity: " + e.entity().name());
                    needsAddress.add(e.entity());
                    return null;
                }

            });
        }
        System.err.println("collectAddressTakenVars needsAddress: "
                + needsAddress.stream().map(Entity::name).collect(Collectors.toList()));
    }

    /**
     * 活跃变量分析
     */
    private void analyzeLiveVariables(List<Stmt> statements) {
        liveBefore.clear();
        liveAfter.clear();
        // 初始化所有语句的活跃变量集合
        for (Stmt stmt : statements) {
            liveBefore.put(stmt, new HashSet<>());
            liveAfter.put(stmt, new HashSet<>());
        }

        // 反向遍历，计算活跃变量
        Set<Entity> live = new HashSet<>();

        for (int i = statements.size() - 1; i >= 0; i--) {
            Stmt stmt = statements.get(i);

            // 计算语句后的活跃变量
            liveAfter.put(stmt, new HashSet<>(live));

            // 更新活跃变量集合
            live.removeAll(defs(stmt)); // 移除被定义的变量
            live.addAll(uses(stmt)); // 添加被使用的变量

            // 计算语句前的活跃变量
            liveBefore.put(stmt, new HashSet<>(live));
        }
    }

    /**
     * 获取语句中定义的变量
     */
    private Set<Entity> defs(Stmt stmt) {
        Set<Entity> defs = new HashSet<>();

        if (stmt instanceof Assign) {
            Assign assign = (Assign) stmt;
            if (assign.lhs() instanceof Var) {
                defs.add(((Var) assign.lhs()).entity());
            }
        }

        return defs;
    }

    /**
     * 获取语句中使用的变量
     */
    private Set<Entity> uses(Stmt stmt) {
        Set<Entity> uses = new HashSet<>();

        if (stmt instanceof Assign) {
            Assign assign = (Assign) stmt;
            collectUses(assign.rhs(), uses);
            // 如果RHS是Call，还需要收集Call的参数
            if (assign.rhs() instanceof Call) {
                Call call = (Call) assign.rhs();
                for (Expr arg : call.args()) {
                    collectUses(arg, uses);
                }
            }
        } else if (stmt instanceof ExprStmt) {
            ExprStmt exprStmt = (ExprStmt) stmt;
            collectUses(exprStmt.expr(), uses);
            // 如果表达式是Call，还需要收集Call的参数
            if (exprStmt.expr() instanceof Call) {
                Call call = (Call) exprStmt.expr();
                if (!call.isStaticCall()) {
                    collectUses(call.expr(), uses);
                }
                for (Expr arg : call.args()) {
                    collectUses(arg, uses);
                }
            }
        } else if (stmt instanceof CJump) {
            CJump cjump = (CJump) stmt;
            collectUses(cjump.cond(), uses);
        } else if (stmt instanceof Return) {
            Return ret = (Return) stmt;
            if (ret.expr() != null) {
                collectUses(ret.expr(), uses);
            }
        }

        return uses;
    }

    /**
     * 收集表达式中使用的变量
     */
    private void collectUses(Expr expr, Set<Entity> uses) {
        if (expr instanceof Var) {
            uses.add(((Var) expr).entity());
        } else if (expr instanceof Addr) {
            uses.add(((Addr) expr).entity());
        } else if (expr instanceof Mem) {
            collectUses(((Mem) expr).expr(), uses);
        } else if (expr instanceof Bin) {
            Bin bin = (Bin) expr;
            collectUses(bin.left(), uses);
            collectUses(bin.right(), uses);
        } else if (expr instanceof Uni) {
            collectUses(((Uni) expr).expr(), uses);
        } else if (expr instanceof Call) {
            Call call = (Call) expr;
            for (Expr arg : call.args()) {
                collectUses(arg, uses);
            }
        }
    }

    /**
     * 构建变量生命周期
     */
    private void buildLifeRanges(List<Stmt> statements) {
        lifeRanges.clear();

        // 遍历语句，累积 start 和 end
        for (int i = 0; i < statements.size(); i++) {
            Stmt stmt = statements.get(i);

            // 定义变量
            for (Entity def : defs(stmt)) {
                LifeRange range = lifeRanges.get(def);
                if (range == null) {
                    range = new LifeRange(def, variableLocalScopeMap.get(def));
                    lifeRanges.put(def, range);
                }
                range.startPoint = (range.startPoint == -1) ? i : Math.min(range.startPoint, i);
                range.endPoint = Math.max(range.endPoint, i);
            }

            // 使用变量
            for (Entity use : uses(stmt)) {
                LifeRange range = lifeRanges.get(use);
                if (range == null) {
                    range = new LifeRange(use, variableLocalScopeMap.get(use));
                    lifeRanges.put(use, range);
                }
                range.startPoint = (range.startPoint == -1) ? i : Math.min(range.startPoint, i);
                range.endPoint = Math.max(range.endPoint, i);
            }
        }

        // 检查跨调用
        for (int i = 0; i < statements.size(); i++) {
            Stmt stmt = statements.get(i);
            boolean isCallStmt = (stmt instanceof ExprStmt && ((ExprStmt) stmt).expr() instanceof Call) ||
                    (stmt instanceof Assign && ((Assign) stmt).rhs() instanceof Call);

            if (isCallStmt) {
                for (LifeRange range : lifeRanges.values()) {
                    if (range.startPoint <= i && range.endPoint >= i) {
                        range.crossesCall = true; // |= true
                    }
                }
            }
        }
    }

    public void adjustSpill(
            Map<Entity, Long> paramOffsets,
            Map<Entity, Long> localVarOffsets) {
        spillOffsets.putAll(localVarOffsets);
        spillOffsets.putAll(paramOffsets);
    }

    /**
     * 分配寄存器
     */
    /**
     * 带溢出策略的寄存器分配
     */
    private void allocateRegisters(
            Map<Entity, Long> localVarOffsets,
            Map<Entity, Long> paramOffsets,
            List<Parameter> parameters,
            List<Variable> globalVariable) {
        // 按生命周期长度和是否跨越函数调用排序
        List<LifeRange> sortedRanges = new ArrayList<>(lifeRanges.values());
        spillOffsets.putAll(localVarOffsets);
        spillOffsets.putAll(paramOffsets);
        sortedRanges.sort((a, b) -> {
            // 优先分配跨越函数调用的变量到callee-saved寄存器
            if (a.crossesCall != b.crossesCall) {
                return a.crossesCall ? -1 : 1;
            }
            // 其次按生命周期长度排序
            int aLength = a.endPoint - a.startPoint;
            int bLength = b.endPoint - b.startPoint;
            return Integer.compare(bLength, aLength);
        });
        System.err.println("paramsOffsets.size = " + paramOffsets.size());

        // 分配寄存器
        for (LifeRange range : sortedRanges) {
            // 如果对其中一个Entity取了地址，那么就不分配寄存器
            // 如果这个Entity是参数，暂时也不分配寄存器,原因是因为这里还没有处理好参数保存到寄存器的逻辑
            System.err.println("try allocate for " + range.entity.name() + " range: [" + range.startPoint + ","
                    + range.endPoint + "] " + " paramsOffsets.containKey = " + parameters.contains(range.entity)
                    + " global.containKey = " + globalVariable.contains(range.entity));
            if (needsAddress.contains(range.entity) || parameters.contains(range.entity)
                    || globalVariable.contains(range.entity))
                continue;

            Register reg = findReusableRegister(range);
            if (reg != null) {
                registerMap.put(range.entity, reg);
                System.err.println("Allocated register " + reg.name() + " for " + range.entity.name());
            } else {
                // 尝试强制溢出策略
                reg = forceAllocateWithSpill(range);
                if (reg != null) {
                    registerMap.put(range.entity, reg);
                    System.err.println(
                            "Force allocated register " + reg.name() + " for " + range.entity.name() + " with spill");
                } else {
                    // 最后手段：溢出到栈上
                    spillToStack(range.entity);
                    System.err.println("Spilled " + range.entity.name() + " to stack");
                }
            }
        }
    }

    /**
     * 强制分配寄存器（通过溢出多个变量）
     */
    private Register forceAllocateWithSpill(LifeRange newRange) {
        Set<Register> candidateRegisters = new LinkedHashSet<>();

        // 确定候选寄存器池
        if (newRange.crossesCall) {
            candidateRegisters.addAll(Arrays.asList(CALLEE_SAVED));
            candidateRegisters.addAll(Arrays.asList(CALLER_SAVED));
        } else {
            candidateRegisters.addAll(Arrays.asList(CALLER_SAVED));
            candidateRegisters.addAll(Arrays.asList(CALLEE_SAVED));
        }

        // 对于每个候选寄存器，尝试通过溢出多个变量来释放它
        for (Register reg : candidateRegisters) {
            List<Entity> entitiesToSpill = findMultipleEntitiesToSpill(reg, newRange);
            if (entitiesToSpill != null && !entitiesToSpill.isEmpty()) {
                // 执行多重溢出
                System.err.println("Force spilling multiple entities to free register " + reg.name() + " for "
                        + newRange.entity.name());
                for (Entity entity : entitiesToSpill) {
                    System.err.println("  - Spilling " + entity.name());
                    spillToStack(entity);
                    registerMap.remove(entity);
                }
                return reg;
            }
        }

        return null;
    }

    /**
     * 查找可以溢出的多个变量来释放指定寄存器
     */
    private List<Entity> findMultipleEntitiesToSpill(Register targetReg, LifeRange newRange) {
        List<Entity> candidates = new ArrayList<>();

        // 收集所有占用目标寄存器的变量
        for (Map.Entry<Entity, Register> entry : registerMap.entrySet()) {
            Entity existingEntity = entry.getKey();
            Register existingRegister = entry.getValue();

            if (!existingRegister.equals(targetReg)) {
                continue;
            }

            LifeRange existingRange = lifeRanges.get(existingEntity);

            // 检查是否可以溢出这个变量
            if (canSpillEntity(existingEntity, existingRange, newRange)) {
                candidates.add(existingEntity);
            }
        }

        // 如果只有一个候选，直接返回
        if (candidates.size() == 1) {
            return candidates;
        }

        // 如果有多个候选，选择最优的组合
        if (candidates.size() > 1) {
            // 按溢出分数排序，选择分数最高的
            candidates.sort((a, b) -> {
                LifeRange rangeA = lifeRanges.get(a);
                LifeRange rangeB = lifeRanges.get(b);
                int scoreA = calculateSpillScore(a, rangeA, newRange);
                int scoreB = calculateSpillScore(b, rangeB, newRange);
                return Integer.compare(scoreB, scoreA); // 降序排列
            });

            // 返回分数最高的候选
            return Arrays.asList(candidates.get(0));
        }

        return null;
    }

    /**
     * 将变量溢出到栈上
     */
    /**
     * 将变量溢出到栈上（支持重复spill）
     */
    private void spillToStack(Entity entity) {
        // 获取当前分配给该实体的寄存器
        Register reg = registerMap.get(entity);
        if (reg == null) {
            // 如果没有分配寄存器，使用简单的偏移量计算
            long offset = nextSpillOffset;
            nextSpillOffset -= 8L;
            spillOffsets.put(entity, offset);
            System.err.println("Spilled " + entity.name() + " to offset " + offset);
            return;
        }

        // 支持重复spill：将寄存器内容压入栈
        long offset = nextSpillOffset;
        nextSpillOffset -= 8L;

        // 初始化该寄存器的spill栈（如果不存在）
        registerSpillStack.computeIfAbsent(reg, k -> new Stack<>()).push(offset);

        // 记录最新的spill偏移量
        spillOffsets.put(entity, offset);

        System.err.println("Spilled " + entity.name() + " (register " + reg.name() + ") to offset " + offset);
    }

    /**
     * 获取寄存器的spill栈（支持重复spill）
     */
    public Stack<Long> getRegisterSpillStack(Register reg) {
        return registerSpillStack.get(reg);
    }

    /**
     * 获取寄存器最新的spill偏移量
     */
    public Long getLatestSpillOffset(Register reg) {
        Stack<Long> stack = registerSpillStack.get(reg);
        return stack != null && !stack.isEmpty() ? stack.peek() : null;
    }

    /**
     * 弹出寄存器的spill栈（恢复寄存器内容）
     */
    public Long popRegisterSpillStack(Register reg) {
        Stack<Long> stack = registerSpillStack.get(reg);
        if (stack != null && !stack.isEmpty()) {
            Long offset = stack.pop();
            System.err.println("Popped spill offset " + offset + " for register " + reg.name());
            return offset;
        }
        return null;
    }

    /**
     * 检查寄存器是否有spill历史
     */
    public boolean hasSpillHistory(Register reg) {
        Stack<Long> stack = registerSpillStack.get(reg);
        return stack != null && !stack.isEmpty();
    }

    /**
     * 获取寄存器的spill深度
     */
    public int getSpillDepth(Register reg) {
        Stack<Long> stack = registerSpillStack.get(reg);
        return stack != null ? stack.size() : 0;
    }

    /**
     * 获取当前正在使用的caller-saved寄存器
     */
    public Set<Register> getUsedCallerSavedRegisters() {
        Set<Register> usedCallerSaved = new HashSet<>();
        for (Map.Entry<Entity, Register> entry : registerMap.entrySet()) {
            Register reg = entry.getValue();
            if (isCallerSaved(reg)) {
                usedCallerSaved.add(reg);
            }
        }
        return usedCallerSaved;
    }

    /**
     * 获取在特定语句点活跃的caller-saved寄存器
     * 这个方法用于在函数调用时只保存和恢复实际活跃的caller-saved寄存器
     */
    public Set<Register> getLiveCallerSavedRegisters(Stmt stmt) {
        Set<Register> liveCallerSaved = new HashSet<>();

        // 获取语句后的活跃变量
        Set<Entity> liveEntities = liveBefore.get(stmt);
        if (liveEntities != null) {
            System.err.println("getLiveCallerSavedRegisters not empty: before"
                    + liveEntities.stream().map(Entity::name).collect(Collectors.toList()));
            Set<Entity> liveEntitiesAfter = liveAfter.get(stmt);
            if (liveEntitiesAfter != null) {
                System.err.println("getLiveCallerSavedRegisters not empty: after"
                        + liveEntitiesAfter.stream().map(Entity::name).collect(Collectors.toList()));
            }
            for (Entity entity : liveEntities) {
                Register reg = registerMap.get(entity);
                if (reg != null && isCallerSaved(reg)) {
                    liveCallerSaved.add(reg);
                }
            }
        } else {
            System.err.println("getLiveCallerSavedRegisters empty");
        }

        return liveCallerSaved;
    }

    /**
     * 检查寄存器是否为caller-saved
     */
    private boolean isCallerSaved(Register reg) {
        for (Register callerSaved : CALLER_SAVED) {
            if (reg.name().equals(callerSaved.name())) {
                return true;
            }
        }
        return false;
    }

    /**
     * 动态重新分配寄存器（在代码生成过程中调用）
     */
    public Register allocateRegisterWithSpill(Entity entity) {
        LifeRange range = lifeRanges.get(entity);
        if (range == null) {
            return null;
        }

        // 首先尝试直接分配
        Register reg = findAvailableRegister(range, getAllCandidateRegisters());
        if (reg != null) {
            registerMap.put(entity, reg);
            return reg;
        }

        // 尝试通过溢出获得寄存器
        reg = findRegisterWithSpill(range, getAllCandidateRegisters());
        if (reg != null) {
            registerMap.put(entity, reg);
            return reg;
        }

        // 最后手段：溢出当前变量
        spillToStack(entity);
        return null;
    }

    /**
     * 获取所有候选寄存器
     */
    private Set<Register> getAllCandidateRegisters() {
        Set<Register> allRegisters = new LinkedHashSet<>();
        allRegisters.addAll(Arrays.asList(CALLER_SAVED));
        allRegisters.addAll(Arrays.asList(CALLEE_SAVED));
        return allRegisters;
    }

    /**
     * 释放寄存器并处理溢出恢复
     */
    public void releaseRegister(Entity entity) {
        Register reg = registerMap.remove(entity);
        if (reg != null) {
            // 检查是否有其他变量可以重新分配到这个寄存器
            tryReallocateRegister(reg);
        }
    }

    /**
     * 尝试重新分配释放的寄存器
     */
    private void tryReallocateRegister(Register freedReg) {
        // 查找可以重新分配到这个寄存器的变量
        for (Map.Entry<Entity, Long> entry : spillOffsets.entrySet()) {
            Entity spilledEntity = entry.getKey();
            LifeRange range = lifeRanges.get(spilledEntity);

            if (range != null && canAllocateToRegister(spilledEntity, freedReg, range)) {
                // 重新分配寄存器
                registerMap.put(spilledEntity, freedReg);
                spillOffsets.remove(spilledEntity);
                System.err.println("Reallocated " + spilledEntity.name() + " to " + freedReg.name());
                break;
            }
        }
    }

    /**
     * 检查变量是否可以分配到指定寄存器
     */
    private boolean canAllocateToRegister(Entity entity, Register reg, LifeRange range) {
        // 检查寄存器是否被其他变量占用
        for (Map.Entry<Entity, Register> entry : registerMap.entrySet()) {
            Entity existingEntity = entry.getKey();
            Register existingRegister = entry.getValue();

            if (existingRegister.equals(reg)) {
                LifeRange existingRange = lifeRanges.get(existingEntity);
                if (existingRange.overlaps(range) || scopesOverlap(existingRange.scope, range.scope)) {
                    return false;
                }
            }
        }
        return true;
    }

    /**
     * 生成函数序言，保存callee-saved寄存器
     */
    private void generatePrologue(DefinedFunction func) {
        // 这里只是标记，实际的代码生成在CodeGenerator中
        // 需要保存所有被使用的callee-saved寄存器
    }

    /**
     * 获取变量的寄存器
     */
    public Register getRegister(Entity entity) {
        return registerMap.get(entity);
    }

    /**
     * 获取变量的溢出偏移
     */
    public Long getSpillOffset(Entity entity) {
        return spillOffsets.get(entity);
    }

    /**
     * 检查变量是否在寄存器中
     */
    public boolean isInRegister(Entity entity) {
        return registerMap.containsKey(entity);
    }

    /**
     * 检查变量是否溢出到栈上
     */
    public boolean isSpilled(Entity entity) {
        return spillOffsets.containsKey(entity);
    }

    /**
     * 获取所有分配的寄存器
     */
    public Set<net.loveruby.cflat.asm.Register> getAllocatedRegisters() {
        return new HashSet<>(registerMap.values());
    }

    public List<Register> getAllocatedRegisterOrderedList() {
        return registerMap.values().stream()
                .filter(register -> {
                    for (Register calleeSaved : CALLEE_SAVED) {
                        if (register.name().equals(calleeSaved.name())) {
                            return true;
                        }
                    }
                    return false;
                })
                .sorted(Comparator.comparing(Register::name)).collect(Collectors.toList());
    }

    /**
     * 获取所有callee-saved寄存器
     */
    public Set<net.loveruby.cflat.asm.Register> getCalleeSavedRegisters() {
        Set<net.loveruby.cflat.asm.Register> used = new HashSet<>();
        for (Register reg : CALLEE_SAVED) {
            if (!availableCalleeSaved.contains(reg)) {
                used.add(reg);
            }
        }
        return used;
    }

    /**
     * 变量生命周期
     */
    private static class LifeRange {
        Entity entity;
        int startPoint;
        int endPoint;
        boolean crossesCall;
        LocalScope scope;

        LifeRange(Entity entity, LocalScope scope) {
            this.entity = entity;
            this.startPoint = -1;
            this.endPoint = -1;
            this.crossesCall = false;
            this.scope = scope;
        }

        public boolean overlaps(LifeRange other) {
            return !(this.endPoint < other.startPoint || other.endPoint < this.startPoint);
        }
    }

    private boolean scopesOverlap(LocalScope a, LocalScope b) {
        return isAncestor(a, b) || isAncestor(b, a);
    }

    private boolean isAncestor(Scope ancestor, Scope child) {
        while (child != null) {
            if (child == ancestor)
                return true;
            child = child.parent();
        }
        return false;
    }

    /**
     * 查找可重用的寄存器，如果找不到则尝试溢出其他变量
     */
    private Register findReusableRegister(LifeRange newRange) {
        Set<Register> candidateRegisters = new LinkedHashSet<>();

        // 1. 确定优先的候选寄存器池（caller-saved 或 callee-saved）
        if (newRange.crossesCall) {
            candidateRegisters.addAll(Arrays.asList(CALLEE_SAVED));
            candidateRegisters.addAll(Arrays.asList(CALLER_SAVED));
        } else {
            candidateRegisters.addAll(Arrays.asList(CALLER_SAVED));
            candidateRegisters.addAll(Arrays.asList(CALLEE_SAVED));
        }

        // 2. 首先尝试直接找到可重用的寄存器
        Register availableReg = findAvailableRegister(newRange, candidateRegisters);
        if (availableReg != null) {
            return availableReg;
        }

        // 3. 如果找不到可用寄存器，尝试溢出策略
        return findRegisterWithSpill(newRange, candidateRegisters);
    }

    /**
     * 查找可用的寄存器（不进行溢出）
     */
    private Register findAvailableRegister(LifeRange newRange, Set<Register> candidateRegisters) {
        for (Register reg : candidateRegisters) {
            boolean conflict = false;
            for (Map.Entry<Entity, Register> entry : registerMap.entrySet()) {
                Entity existingEntity = entry.getKey();
                Register existingRegister = entry.getValue();

                if (!existingRegister.equals(reg))
                    continue;

                LifeRange existingRange = lifeRanges.get(existingEntity);

                // 存在冲突：
                // (1) 生命周期重叠
                // (2) 或作用域重叠（嵌套或相交）
                System.err.println("existingRange: [" + existingRange.startPoint + "," + existingRange.endPoint + "]"
                        + " scope: " + existingRange.scope);
                System.err.println("newRange: [" + newRange.startPoint + "," + newRange.endPoint + "]" + " scope: "
                        + newRange.scope);
                if (existingRange.overlaps(newRange) || scopesOverlap(existingRange.scope, newRange.scope)) {
                    conflict = true;
                    break;
                }
            }

            if (!conflict) {
                return reg;
            }
        }
        return null;
    }

    /**
     * 通过溢出其他变量来找到可用寄存器
     */
    private Register findRegisterWithSpill(LifeRange newRange, Set<Register> candidateRegisters) {
        // 对于每个候选寄存器，检查是否可以溢出占用它的变量
        for (Register reg : candidateRegisters) {
            Entity entityToSpill = findEntityToSpill(reg, newRange);
            if (entityToSpill != null) {
                // 找到可以溢出的变量，执行溢出
                System.err.println("Spilling " + entityToSpill.name() + " to free register " + reg.name() + " for "
                        + newRange.entity.name());
                spillToStack(entityToSpill);
                registerMap.remove(entityToSpill);
                return reg;
            }
        }
        return null; // 无法通过溢出获得寄存器
    }

    /**
     * 查找可以溢出的变量来释放指定寄存器
     */
    private Entity findEntityToSpill(Register targetReg, LifeRange newRange) {
        Entity bestCandidate = null;
        int bestScore = Integer.MIN_VALUE;

        for (Map.Entry<Entity, Register> entry : registerMap.entrySet()) {
            Entity existingEntity = entry.getKey();
            Register existingRegister = entry.getValue();

            if (!existingRegister.equals(targetReg)) {
                continue;
            }

            LifeRange existingRange = lifeRanges.get(existingEntity);

            // 检查是否可以溢出这个变量
            if (canSpillEntity(existingEntity, existingRange, newRange)) {
                int score = calculateSpillScore(existingEntity, existingRange, newRange);
                if (score > bestScore) {
                    bestScore = score;
                    bestCandidate = existingEntity;
                }
            }
        }

        return bestCandidate;
    }

    /**
     * 检查是否可以溢出指定变量
     */
    private boolean canSpillEntity(Entity entity, LifeRange existingRange, LifeRange newRange) {
        // 不能溢出跨越函数调用的变量（除非新变量也跨越函数调用）
        if (existingRange.crossesCall && !newRange.crossesCall) {
            return false;
        }

        // 不能溢出生命周期完全包含新变量的变量
        if (existingRange.startPoint <= newRange.startPoint && existingRange.endPoint >= newRange.endPoint) {
            return false;
        }

        // 不能溢出地址被取用的变量
        if (needsAddress.contains(entity)) {
            return false;
        }

        return true;
    }

    /**
     * 计算溢出优先级分数（分数越高越适合溢出）
     */
    private int calculateSpillScore(Entity entity, LifeRange existingRange, LifeRange newRange) {
        int score = 0;

        // 1. 生命周期长度：生命周期越短越适合溢出
        int existingLength = existingRange.endPoint - existingRange.startPoint;
        int newLength = newRange.endPoint - newRange.startPoint;
        score += (newLength - existingLength) * 10;

        // 2. 跨越函数调用：不跨越函数调用的变量优先溢出
        if (!existingRange.crossesCall && newRange.crossesCall) {
            score += 50;
        }

        // 3. 使用频率：使用频率低的变量优先溢出
        // 这里简化处理，可以根据实际需要添加更复杂的启发式

        // 4. 作用域嵌套：外层作用域的变量优先溢出
        if (isAncestor(newRange.scope, existingRange.scope)) {
            score += 20;
        }

        return score;
    }

    int estimateSpillOffset(Expr expr) {
        final AtomicInteger size = new AtomicInteger(0);
        expr.accept(new DefaultIRVisitor<Void, Integer>() {
            @Override
            public Integer visit(Uni s) {
                int result = s.expr().accept(this);
                size.set(Math.max(size.get(), result));
                return result;
            }

            @Override
            public Integer visit(Bin s) {
                int left = s.left().accept(this); // 左边表达式需要占据的spill
                int right = s.right().accept(this); // 右边表达式需要占据的spill

                // 二元运算需要：
                // 1. 计算左操作数
                // 2. 计算右操作数（可能需要临时寄存器）
                // 3. 执行运算（可能需要额外的临时寄存器）
                // 4. 保存结果（可能需要临时寄存器）

                // 如果右操作数复杂，可能需要额外的临时寄存器来保存中间结果
                int requiredTempRegs = Math.max(1, right); // 至少需要1个临时寄存器

                // 如果需要的临时寄存器超过可用数量，就需要溢出
                if (requiredTempRegs > totalTempRegisters.length) {
                    requiredTempRegs = totalTempRegisters.length; // 最多溢出所有临时寄存器
                }

                int result = Math.max(left + requiredTempRegs, right);
                size.set(Math.max(size.get(), result));
                return result;
            }

            @Override
            public Integer visit(Call s) {
                int maxSpill = 0;
                int currentSpill = 0;
                for (Expr e : s.args()) {
                    int argSpill = e.accept(this);
                    currentSpill = Math.max(currentSpill, argSpill);
                    maxSpill = Math.max(maxSpill, currentSpill);
                }
                size.set(Math.max(size.get(), maxSpill));
                return maxSpill;
            }

            @Override
            public Integer visit(Addr s) {
                return 0;
            }

            @Override
            public Integer visit(Mem s) {
                int item = s.expr().accept(this);
                size.set(Math.max(size.get(), item));
                return item;
            }

            @Override
            public Integer visit(Var s) {
                // 变量都在本地栈中有空间，不需要考虑溢出
                return 0;
            }

            @Override
            public Integer visit(Int s) {
                return 0;
            }

            @Override
            public Integer visit(Str s) {
                return 0;
            }
        });
        return size.get();
    };

    int estimateSpillOffset(Assign stmt) {
        int right = estimateSpillOffset(stmt.rhs());
        int left = estimateSpillOffset(stmt.lhs());
        // left+1是因为right的结果必须要保留
        return Math.max(left + 1, right);
    }

    int estimateSpillOffset(CJump stmt) {
        return estimateSpillOffset(stmt.cond());
    }

    int estimateSpillOffset(ExprStmt stmt) {
        return estimateSpillOffset(stmt.expr());
    }

    int estimateSpillOffset(Jump stmt) {
        return 0;
    }

    int estimateSpillOffset(Return stmt) {
        if (stmt.expr() != null) {
            return estimateSpillOffset(stmt.expr());
        } else {
            return 0;
        }
    }

    int estimateSpillOffset(Switch stmt) {
        return estimateSpillOffset(stmt.cond());
    }

    int estimateSpillOffset(DefinedFunction f) {
        AtomicInteger size = new AtomicInteger(0);
        for (Stmt stmt : f.ir()) {
            stmt.accept(new DefaultIRVisitor<Void, Void>() {
                @Override
                public Void visit(ExprStmt s) {
                    size.set(estimateSpillOffset(s));
                    return null;
                }

                @Override
                public Void visit(Assign s) {
                    size.set(estimateSpillOffset(s));
                    return null;
                }

                @Override
                public Void visit(CJump s) {
                    size.set(estimateSpillOffset(s));
                    return null;
                }

                @Override
                public Void visit(Jump s) {
                    size.set(estimateSpillOffset(s));
                    return null;
                }

                @Override
                public Void visit(Switch s) {
                    size.set(estimateSpillOffset(s));
                    return null;
                }

                @Override
                public Void visit(LabelStmt s) {
                    size.set(0);
                    return null;
                }

                @Override
                public Void visit(Return s) {
                    size.set(estimateSpillOffset(s));
                    return null;
                }
            });
        }

        // 计算表达式求值过程中需要的临时栈槽数量
        int exprSpillSlots = size.get();

        // 为临时寄存器溢出分配空间：当表达式复杂到需要超过可用临时寄存器数量时
        // 每个临时寄存器都可能需要溢出，所以需要为所有临时寄存器分配溢出空间
        int tempSpillSlots = totalTempRegisters.length;

        return exprSpillSlots + tempSpillSlots;
    }
}