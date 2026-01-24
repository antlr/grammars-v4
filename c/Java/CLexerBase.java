import org.antlr.v4.runtime.*;
import org.antlr.v4.runtime.misc.Interval;
import java.io.*;
import java.nio.file.*;
import java.util.*;

public abstract class CLexerBase extends Lexer {

    public CLexerBase(CharStream input) {
        super(runGccAndMakeStream(input));
    }

    public static CharStream runGccAndMakeStream(CharStream input) {
        boolean isWindows = System.getProperty("os.name").toLowerCase().contains("win");

        // Get options from system property or environment
        String[] args = getCommandLineArgs();

        boolean vsc = hasArg(args, "--vsc");
        boolean gcc = hasArg(args, "--gcc");
        boolean clang = hasArg(args, "--clang");
        boolean nopp = hasArg(args, "--nopp");

        if (!(vsc || gcc || clang)) {
            gcc = true;
        }

        // Extract preprocessor options (-D and -I)
        List<String> ppOptions = extractPreprocessorOptions(args);

        // Replace input with preprocessed input.
        String sourceName = input.getSourceName();
        if (sourceName == null || !sourceName.endsWith(".c")) {
            sourceName = "stdin.c";
        }
        String outputName = sourceName + ".p";
        String inputText = input.getText(new Interval(0, input.size() - 1));

        if (nopp) {
            try {
                Files.writeString(Path.of(outputName), inputText);
            } catch (IOException e) {
                // Ignore
            }
            return CharStreams.fromString(inputText);
        }

        try {
            Files.writeString(Path.of(outputName), inputText);
        } catch (IOException e) {
            // Ignore
        }

        if (gcc) {
            try {
                String gccCommand = isWindows ? "gcc.exe" : "gcc";
                List<String> command = new ArrayList<>();
                command.add(gccCommand);
                command.add("-std=c2x");
                command.add("-E");
                command.add("-C");
                // Add preprocessor options (-D and -I)
                command.addAll(ppOptions);
                command.add(sourceName);
                ProcessBuilder pb = new ProcessBuilder(command);
                pb.redirectErrorStream(false);
                Process process = pb.start();

                process.getOutputStream().close(); // signal EOF

                String output = new String(process.getInputStream().readAllBytes());
                String error = new String(process.getErrorStream().readAllBytes());

                process.waitFor();

                Files.writeString(Path.of(outputName), output);
                return CharStreams.fromString(output);
            } catch (Exception e) {
                throw new RuntimeException("Failed to run gcc preprocessor", e);
            }
        }

        if (clang) {
            try {
                String clangCommand = isWindows ? "clang.exe" : "clang";
                List<String> command = new ArrayList<>();
                command.add(clangCommand);
                command.add("-std=c2x");
                command.add("-E");
                command.add("-C");
                // Add preprocessor options (-D and -I)
                command.addAll(ppOptions);
                command.add(sourceName);
                ProcessBuilder pb = new ProcessBuilder(command);
                pb.redirectErrorStream(false);
                Process process = pb.start();

                process.getOutputStream().close();

                String output = new String(process.getInputStream().readAllBytes());
                String error = new String(process.getErrorStream().readAllBytes());

                process.waitFor();

                Files.writeString(Path.of(outputName), output);
                return CharStreams.fromString(output);
            } catch (Exception e) {
                throw new RuntimeException("Failed to run clang preprocessor", e);
            }
        }

        // Note: vsc (Visual Studio cl.exe) support is Windows-specific and complex
        // Skipping for Java port as it requires Windows-specific path resolution

        throw new RuntimeException("No preprocessor specified.");
    }

    private static String[] getCommandLineArgs() {
        // Try to get command line args from system property
        String cmdLine = System.getProperty("sun.java.command");
        if (cmdLine != null) {
            return cmdLine.split("\\s+");
        }
        return new String[0];
    }

    private static boolean hasArg(String[] args, String arg) {
        for (String a : args) {
            if (a.toLowerCase().contains(arg.toLowerCase())) {
                return true;
            }
        }
        return false;
    }

    private static List<String> extractPreprocessorOptions(String[] args) {
        List<String> options = new ArrayList<>();
        if (args == null) return options;

        for (String arg : args) {
            // Match --D and --I options
            if (arg.startsWith("--D")) {
                options.add("-D" + arg.substring(3));
            }
            else if (arg.startsWith("--I")) {
                options.add("-I" + arg.substring(3));
            }
        }
        return options;
    }
}
