package net.loveruby.cflat.sysdep;

import net.loveruby.cflat.utils.CommandUtils;
import net.loveruby.cflat.utils.ErrorHandler;
import net.loveruby.cflat.exception.IPCException;
import java.util.List;
import java.util.ArrayList;

class MacOSLinker implements Linker {
    // macOS specific linker settings
    static final private String LINKER = "/usr/bin/ld";

    ErrorHandler errorHandler;

    MacOSLinker(ErrorHandler errorHandler) {
        this.errorHandler = errorHandler;
    }

    public void generateExecutable(List<String> args,
            String destPath, LinkerOptions opts) throws IPCException {
        List<String> cmd = new ArrayList<String>();
        cmd.add(LINKER);
        cmd.add("-arch");
        cmd.add("arm64");
        cmd.add("-macos_version_min");
        cmd.add("11.0");

        // macOS doesn't use -dynamic-linker
        if (opts.generatingPIE) {
            cmd.add("-pie");
        }

        // Don't use crt1.o for now to avoid architecture issues
        // if (! opts.noStartFiles) {
        // cmd.add(C_RUNTIME_START);
        // }

        cmd.addAll(args);

        if (!opts.noDefaultLibs) {
            cmd.add("-L/Library/Developer/CommandLineTools/SDKs/MacOSX14.4.sdk/usr/lib");
            cmd.add("-lc");
            cmd.add("-lcbc"); // Removed
        }

        cmd.add("-o");
        cmd.add(destPath);

        CommandUtils.invoke(cmd, errorHandler, opts.verbose);
    }

    public void generateSharedLibrary(List<String> args,
            String destPath, LinkerOptions opts) throws IPCException {
        List<String> cmd = new ArrayList<String>();
        cmd.add(LINKER);
        cmd.add("-arch");
        cmd.add("arm64");
        cmd.add("-macos_version_min");
        cmd.add("11.0");
        cmd.add("-shared");

        cmd.addAll(args);

        if (!opts.noDefaultLibs) {
            cmd.add("-L/Library/Developer/CommandLineTools/SDKs/MacOSX14.4.sdk/usr/lib");
            cmd.add("-lc");
            // cmd.add("-lcbc"); // Removed
        }

        cmd.add("-o");
        cmd.add(destPath);

        CommandUtils.invoke(cmd, errorHandler, opts.verbose);
    }
}