package net.loveruby.cflat.sysdep.arm64;

import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.utils.ErrorHandler;
import java.util.*;
import java.io.PrintStream;

public class AssemblyCode implements net.loveruby.cflat.sysdep.AssemblyCode {
    private List<Assembly> assemblies = new ArrayList<Assembly>();
    private ErrorHandler errorHandler;

    public AssemblyCode(ErrorHandler h) {
        this.errorHandler = h;
    }

    public void add(Assembly asm) {
        assemblies.add(asm);
    }

    public List<Assembly> assemblies() {
        return assemblies;
    }

    public void collectStatistics(Statistics stats) {
        for (Assembly asm : assemblies) {
            asm.collectStatistics(stats);
        }
    }

    public String toSource() {
        StringBuilder buf = new StringBuilder();

        for (Assembly asm : assemblies) {
            buf.append(asm.toSource(SymbolTable.dummy()));
            buf.append("\n");
        }

        return buf.toString();
    }

    public void dump() {
        dump(System.out);
    }

    public void dump(PrintStream s) {
        s.println(toSource());
    }

    public void write(String path) {
        // Implementation for writing assembly to file
        // This would be similar to the x86 implementation
    }
}