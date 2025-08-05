package net.loveruby.cflat.sysdep.arm64;

import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.ir.*;

import java.util.*;
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
//            net.loveruby.cflat.sysdep.arm64.Register.X9,
            net.loveruby.cflat.sysdep.arm64.Register.X10,
//            net.loveruby.cflat.sysdep.arm64.Register.X11,
//            net.loveruby.cflat.sysdep.arm64.Register.X12,
//            net.loveruby.cflat.sysdep.arm64.Register.X13,
            net.loveruby.cflat.sysdep.arm64.Register.X14,
            net.loveruby.cflat.sysdep.arm64.Register.X15
    };

    // 当前可用的寄存器
    private Set<net.loveruby.cflat.sysdep.arm64.Register> availableCalleeSaved = new LinkedHashSet<>(Arrays.asList(CALLEE_SAVED));
    private Set<Register> availableCallerSaved = new LinkedHashSet<>(Arrays.asList(CALLER_SAVED));

    // 变量到寄存器的映射
    private Map<Entity, net.loveruby.cflat.sysdep.arm64.Register> registerMap = new LinkedHashMap<>();

    // 溢出到栈上的变量
    private Map<Entity, Long> spillOffsets = new HashMap<>();

    // 活跃变量分析结果
    private final Map<Stmt, Set<Entity>> liveBefore = new HashMap<>();
    private final Map<Stmt, Set<Entity>> liveAfter = new HashMap<>();
    private final Set<Entity> needsAddress = new LinkedHashSet<>();

    // 变量生命周期
    private final Map<Entity, LifeRange> lifeRanges = new HashMap<>();
    /* 设计这个的目的是为了将寄存器分配逻辑完全放到CodeGenerator中，CodeGenerator不再保留任何硬编码代码 */
    private final Register[] totalTempRegisters = {
            Register.X9,
            Register.X16
    };
    private final Set<Register> tempAvaiableRegisterList = new LinkedHashSet<>(Arrays.asList(
            totalTempRegisters
    ));

    public Register allocateTempRegister() {
        if (tempAvaiableRegisterList.isEmpty()) {
            throw new IllegalStateException("no temp register");
        }
        Register item = tempAvaiableRegisterList.iterator().next();
        tempAvaiableRegisterList.remove(item);
        return item;
    }

    public void releaseTempRegister(Register register) {
        boolean valid = false;
        for (Register totalTempRegister : totalTempRegisters) {
            if (totalTempRegister.name().equals(register.name())) {
                valid = true;
                break;
            }
        }
        if (valid) {
            tempAvaiableRegisterList.add(register);
        }
    }


    /**
     * 为函数分配寄存器
     */
    public void allocateRegisters(DefinedFunction func,
                                  Map<Entity, Long> localVarOffsets,
                                  Map<Entity, Long> paramOffsets,
                                  List<Variable> globalVariables
    ) {
        registerMap.clear();
        spillOffsets.clear();
        tempAvaiableRegisterList.clear();
        tempAvaiableRegisterList.addAll(Arrays.asList(totalTempRegisters));
        List<Stmt> statements = func.ir();
        // 0. 标记始终从内存中获取值的变量
        collectAddressTakenVars(statements);

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
        System.err.println("collectAddressTakenVars needsAddress: " + needsAddress.stream().map(Entity::name).collect(Collectors.toList()));
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

        // 为每个变量找到定义点和最后使用点
        for (int i = 0; i < statements.size(); i++) {
            Stmt stmt = statements.get(i);

            // 检查变量定义
            for (Entity def : defs(stmt)) {
                LifeRange range = lifeRanges.get(def);
                if (range == null) {
                    range = new LifeRange(def);
                    lifeRanges.put(def, range);
                }
                range.startPoint = i;
            }

            // 检查变量使用
            for (Entity use : uses(stmt)) {
                LifeRange range = lifeRanges.get(use);
                if (range == null) {
                    range = new LifeRange(use);
                    lifeRanges.put(use, range);
                }
                range.endPoint = i;
            }
        }

        // 检查是否跨越函数调用
        for (int i = 0; i < statements.size(); i++) {
            Stmt stmt = statements.get(i);
            if (stmt instanceof ExprStmt && ((ExprStmt) stmt).expr() instanceof Call) {
                // 标记跨越函数调用的变量
                for (LifeRange range : lifeRanges.values()) {
                    if (range.startPoint <= i && range.endPoint >= i) {
                        range.crossesCall = true;
                    }
                }
            } else if (stmt instanceof Assign) {
                Assign assign = (Assign) stmt;
                if (assign.rhs() instanceof Call) {
                    // 标记跨越函数调用的变量
                    for (LifeRange range : lifeRanges.values()) {
                        if (range.startPoint <= i && range.endPoint >= i) {
                            range.crossesCall = true;
                        }
                    }
                }
            }
        }
    }

    public void adjustSpill(
            Map<Entity, Long> paramOffsets,
            Map<Entity, Long> localVarOffsets
    ) {
        spillOffsets.putAll(localVarOffsets);
        spillOffsets.putAll(paramOffsets);
    }

    /**
     * 分配寄存器
     */
    private void allocateRegisters(
            Map<Entity, Long> localVarOffsets,
            Map<Entity, Long> paramOffsets,
            List<Parameter> parameters,
            List<Variable> globalVariable
    ) {
        // 按生命周期长度和是否跨越函数调用排序
        List<LifeRange> sortedRanges = new ArrayList<>(lifeRanges.values());
        spillOffsets.putAll(localVarOffsets);
        spillOffsets.putAll(paramOffsets);
        sortedRanges.sort((a, b) -> {
            // 优先分配跨越函数调用的变量到callee-saved寄存器
//            if (a.crossesCall != b.crossesCall) {
//                return a.crossesCall ? -1 : 1;
//            }
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
            System.err.println("try allocate for " + range.entity.name() + " range: [" + range.startPoint + "," + range.endPoint + "] " + " paramsOffsets.containKey = " + parameters.contains(range.entity) + " global.containKey = " + globalVariable.contains(range.entity));
            if (needsAddress.contains(range.entity) || parameters.contains(range.entity) || globalVariable.contains(range.entity))
                continue;
            Register reg = allocateRegister(range);
            if (reg != null) {
                registerMap.put(range.entity, reg);
            } else {
                // 溢出到栈上
                spillToStack(range.entity);
            }
        }
    }

    /**
     * 为变量分配寄存器
     */
    private net.loveruby.cflat.sysdep.arm64.Register allocateRegister(LifeRange range) {
        if (range.crossesCall) {
            // 跨越函数调用，优先使用callee-saved寄存器
            if (!availableCalleeSaved.isEmpty()) {
                Register reg = availableCalleeSaved.iterator().next();
                availableCalleeSaved.remove(reg);
                return reg;
            } else if (!availableCallerSaved.isEmpty()) {
                Register reg = availableCallerSaved.iterator().next();
                availableCallerSaved.remove(reg);
                return reg;
            }
        } else {
            // 不跨越函数调用，优先使用caller-saved寄存器
            if (!availableCallerSaved.isEmpty()) {
                Register reg = availableCallerSaved.iterator().next();
                availableCallerSaved.remove(reg);
                return reg;
            } else if (!availableCalleeSaved.isEmpty()) {
                Register reg = availableCalleeSaved.iterator().next();
                availableCalleeSaved.remove(reg);
                return reg;
            }
        }

        return null; // 没有可用寄存器，需要溢出
    }

    /**
     * 将变量溢出到栈上
     */
    private void spillToStack(Entity entity) {
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

        LifeRange(Entity entity) {
            this.entity = entity;
            this.startPoint = -1;
            this.endPoint = -1;
            this.crossesCall = false;
        }

        public boolean overlaps(LifeRange other) {
            return !(this.endPoint < other.startPoint || other.endPoint < this.startPoint);
        }
    }
}