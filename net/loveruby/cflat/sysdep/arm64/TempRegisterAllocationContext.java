package net.loveruby.cflat.sysdep.arm64;

import java.util.HashSet;
import java.util.Set;

public class TempRegisterAllocationContext {
    int allocCount = 0;
    final int maxAllocCount;
    public final String source;

    // 跟踪当前context已分配的寄存器
    private final Set<Register> allocatedRegisters = new HashSet<>();

    TempRegisterAllocationContext(int maxAllocCount, String source) {
        System.err.println("TempRegisterAllocationContext maxAllocCount = " + maxAllocCount);
        this.maxAllocCount = maxAllocCount;
        this.source = source;
    }

    void recordAlloc() {
        allocCount++;
        System.err.println("current allocCount = " + allocCount);
        if (allocCount > maxAllocCount) {
            throw new IllegalStateException("you cannot own " + maxAllocCount + "in same function");
        }
    }

    void recordRelease() {
        allocCount--;
    }

    void checkState() {
        if (allocCount != 0) {
            throw new IllegalStateException("do you forget release?");
        }
    }

    /**
     * 检查寄存器是否已经被当前context分配
     */
    boolean isRegisterAllocated(Register register) {
        return allocatedRegisters.contains(register);
    }

    /**
     * 记录寄存器被当前context分配
     */
    void recordRegisterAllocation(Register register) {
        allocatedRegisters.add(register);
        System.err.println("Context " + source + " allocated register: " + register.name());
    }

    /**
     * 记录寄存器被当前context释放
     */
    void recordRegisterRelease(Register register) {
        allocatedRegisters.remove(register);
        System.err.println("Context " + source + " released register: " + register.name());
    }
}
