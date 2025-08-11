package net.loveruby.cflat.sysdep.arm64;

import net.loveruby.cflat.asm.SymbolTable;

public class Register extends net.loveruby.cflat.asm.Register {
    private String name;
    private boolean isGeneral;
    private boolean isCallerSaved;
    private boolean isCalleeSaved;
    private boolean isAllocatable;

    public String name() {
        return name;
    }

    public Register(String name, boolean isGeneral, boolean isCallerSaved,
            boolean isCalleeSaved, boolean isAllocatable) {
        this.name = name;
        this.isGeneral = isGeneral;
        this.isCallerSaved = isCallerSaved;
        this.isCalleeSaved = isCalleeSaved;
        this.isAllocatable = isAllocatable;
    }

    public String toString() {
        return name;
    }

    public String toSource(SymbolTable syms) {
        return name;
    }

    public String dump() {
        return "(Register " + name + ")";
    }

    public boolean isGeneral() {
        return isGeneral;
    }

    public boolean isCallerSaved() {
        return isCallerSaved;
    }

    public boolean isCalleeSaved() {
        return isCalleeSaved;
    }

    public boolean isAllocatable() {
        return isAllocatable;
    }

    public String bit32Name() {
        return "w" + name.substring(1);
    }

    // ARM64 general purpose registers
    public static final Register X0 = new Register("x0", true, true, false, true);
    public static final Register X1 = new Register("x1", true, true, false, true);
    public static final Register X2 = new Register("x2", true, true, false, true);
    public static final Register X3 = new Register("x3", true, true, false, true);
    public static final Register X4 = new Register("x4", true, true, false, true);
    public static final Register X5 = new Register("x5", true, true, false, true);
    public static final Register X6 = new Register("x6", true, true, false, true);
    public static final Register X7 = new Register("x7", true, true, false, true);
    public static final Register X8 = new Register("x8", true, true, false, true);
    public static final Register X9 = new Register("x9", true, true, false, true);
    public static final Register X10 = new Register("x10", true, true, false, true);
    public static final Register X11 = new Register("x11", true, true, false, true);
    public static final Register X12 = new Register("x12", true, true, false, true);
    public static final Register X13 = new Register("x13", true, true, false, true);
    public static final Register X14 = new Register("x14", true, true, false, true);
    public static final Register X15 = new Register("x15", true, true, false, true);
    public static final Register X16 = new Register("x16", true, true, false, true);
    public static final Register X17 = new Register("x17", true, true, false, true);
    public static final Register X18 = new Register("x18", true, false, true, true);
    public static final Register X19 = new Register("x19", true, false, true, true);
    public static final Register X20 = new Register("x20", true, false, true, true);
    public static final Register X21 = new Register("x21", true, false, true, true);
    public static final Register X22 = new Register("x22", true, false, true, true);
    public static final Register X23 = new Register("x23", true, false, true, true);
    public static final Register X24 = new Register("x24", true, false, true, true);
    public static final Register X25 = new Register("x25", true, false, true, true);
    public static final Register X26 = new Register("x26", true, false, true, true);
    public static final Register X27 = new Register("x27", true, false, true, true);
    public static final Register X28 = new Register("x28", true, false, true, true);
    public static final Register X29 = new Register("x29", true, false, true, true); // FP
    public static final Register X30 = new Register("x30", true, true, false, false); // LR
    public static final Register SP = new Register("sp", false, false, false, false); // Stack pointer
    public static final Register XZR = new Register("xzr", false, false, false, false); // Zero register

    // Special registers
    public static final Register FP = X29; // Frame pointer
    public static final Register LR = X30; // Link register

    // Return value register
    public static final Register RV = X0;

    // Argument registers
    public static final Register[] ARGS = { X0, X1, X2, X3, X4, X5, X6, X7 };

    // Caller-saved registers (temporary)
    public static final Register[] CALLER_SAVED = {
            X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17, LR
    };

    // Callee-saved registers
    public static final Register[] CALLEE_SAVED = {
            X19, X20, X21, X22, X23, X24, X25, X26, X27, X28, FP
    };

    // Allocatable registers
    public static final Register[] ALLOCATABLE = {
            X0, X1, X2, X3, X4, X5, X6, X7, X8, X9, X10, X11, X12, X13, X14, X15, X16, X17,
            X18, X19, X20, X21, X22, X23, X24, X25, X26, X27, X28
    };

    // ARM64 floating-point registers
    public static final Register D0 = new Register("d0", true, true, false, true);
    public static final Register D1 = new Register("d1", true, true, false, true);
    public static final Register D2 = new Register("d2", true, true, false, true);
    public static final Register D3 = new Register("d3", true, true, false, true);
    public static final Register D4 = new Register("d4", true, true, false, true);
    public static final Register D5 = new Register("d5", true, true, false, true);
    public static final Register D6 = new Register("d6", true, true, false, true);
    public static final Register D7 = new Register("d7", true, true, false, true);
    public static final Register D8 = new Register("d8", true, false, true, true);
    public static final Register D9 = new Register("d9", true, false, true, true);
    public static final Register D10 = new Register("d10", true, false, true, true);
    public static final Register D11 = new Register("d11", true, false, true, true);
    public static final Register D12 = new Register("d12", true, false, true, true);
    public static final Register D13 = new Register("d13", true, false, true, true);
    public static final Register D14 = new Register("d14", true, false, true, true);
    public static final Register D15 = new Register("d15", true, false, true, true);

    // Floating-point caller-saved registers
    public static final Register[] FLOAT_CALLER_SAVED = {
            D0, D1, D2, D3, D4, D5, D6, D7
    };

    // Floating-point callee-saved registers
    public static final Register[] FLOAT_CALLEE_SAVED = {
            D8, D9, D10, D11, D12, D13, D14, D15
    };

    // All floating-point registers
    public static final Register[] ALL_FLOAT_REGISTERS = {
            D0, D1, D2, D3, D4, D5, D6, D7, D8, D9, D10, D11, D12, D13, D14, D15
    };
}