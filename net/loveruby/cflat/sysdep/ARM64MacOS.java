package net.loveruby.cflat.sysdep;

import net.loveruby.cflat.utils.ErrorHandler;
import net.loveruby.cflat.type.TypeTable;
import net.loveruby.cflat.asm.Type;

public class ARM64MacOS implements Platform {
    public TypeTable typeTable() {
        return TypeTable.lp64(); // ARM64 uses LP64 data model
    }

    public CodeGenerator codeGenerator(
            CodeGeneratorOptions opts, ErrorHandler h) {
        return new net.loveruby.cflat.sysdep.arm64.CodeGenerator(
                opts, naturalType(), h);
    }

    private Type naturalType() {
        return Type.INT64; // ARM64 natural type is 64-bit
    }

    public Assembler assembler(ErrorHandler h) {
        return new GNUAssembler(h);
    }

    public Linker linker(ErrorHandler h) {
        return new MacOSLinker(h);
    }
}