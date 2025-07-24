package net.loveruby.cflat.sysdep.arm64;


import java.util.*;

/**
 * 极简可溢出寄存器池：
 *  - 只管理 caller-saved 通用寄存器（不含 x0~x7 参数/返回寄存器，x29/x30 FP/LR，sp）。
 *  - 当寄存器用完时自动把最早借出的一个值 spill 到栈上，再返回它的寄存器。
 *  - 释放(reg) 时，如果该寄存器对应有 spill，则不自动 reload（由调用方决定是否需要）。
 *
 * 使用方式：
 *  - acquire(): 拿一个可用寄存器
 *  - release(reg): 释放
 *  - spillAllToStack(): 把当前池中所有寄存器的值依次 spill
 *
 * 为了简单，spill 区域由使用者预留一块“temps”区域，通过 setSpillBase() / nextSpillOffset() 获取偏移。
 */
public class RegPool {

    // 建议排除 x0~x7, x29, x30
    private static final Register[] CANDIDATES = {
            Register.X9, Register.X10, Register.X11, Register.X12,
            Register.X13, Register.X14, Register.X15, Register.X16,
            Register.X17, Register.X18
    };

    private final Deque<Register> free = new ArrayDeque<>();
    private final Deque<Register> inUse = new ArrayDeque<>();

    private long spillBase;          // sp + spillBase 是第一个spill槽位
    private long spillNextOffset;    // 递增
    private final Map<Register, Long> spilledAt = new HashMap<>();

    public RegPool() {
        free.addAll(Arrays.asList(CANDIDATES));
    }

    public void reset(long spillBaseOffset) {
        free.clear();
        free.addAll(Arrays.asList(CANDIDATES));
        inUse.clear();
        spilledAt.clear();
        spillBase = spillBaseOffset;
        spillNextOffset = 0;
    }

    public Register acquire() {
        Register r;
        if (!free.isEmpty()) {
            r = free.removeFirst();
            inUse.addLast(r);
            return r;
        }
        // 溢出最早借出的
        r = inUse.removeFirst();
        long off = spillNextOffset;
        spillNextOffset += 8;
        spilledAt.put(r, off);
        // 调用方负责生成 "str r, [sp, #spillBase+off]"
        inUse.addLast(r);
        return r;
    }

    public boolean isSpilled(Register r) {
        return spilledAt.containsKey(r);
    }

    public long spillOffset(Register r) {
        return spillBase + spilledAt.get(r);
    }

    public void unspill(Register r) {
        spilledAt.remove(r);
    }

    /** 释放一个寄存器 */
    public void release(Register r) {
        if (!inUse.remove(r)) return;
        free.addFirst(r);
        spilledAt.remove(r);
    }

    /** 把所有 inUse 的寄存器spill（调用方需生成具体 str 指令） */
    public List<Register> spillAllOrder() {
        List<Register> out = new ArrayList<>(inUse);
        for (Register r : out) {
            if (!spilledAt.containsKey(r)) {
                long off = spillNextOffset;
                spillNextOffset += 8;
                spilledAt.put(r, off);
            }
        }
        return out;
    }

    public Collection<Register> liveRegs() {
        return Collections.unmodifiableCollection(inUse);
    }

    public long totalSpillSize() {
        return spillNextOffset;
    }
}
