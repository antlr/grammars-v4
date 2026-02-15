using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using Antlr4.Runtime.Misc;

public abstract class CLexerBase : Lexer
{
    public CLexerBase(ICharStream input)
        : base(RunGccAndMakeStream(input))
    {
    }

    public CLexerBase(ICharStream i, TextWriter o, TextWriter errorOutput)
        : base(RunGccAndMakeStream(i), o, errorOutput)
    {
    }

    public static ICharStream RunGccAndMakeStream(ICharStream i)
    {
        var is_windows = false;
        var os = System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform;
        if (os(System.Runtime.InteropServices.OSPlatform.Windows))
        {
            is_windows = true;
        }

        // Get options to lexer from process args.
        var args = Environment.GetCommandLineArgs().ToList();

        // Determine which preprocessor to run: gcc or cl.exe or clang.
        var vsc = args?.Where(a => a.IndexOf("--vsc", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        var gcc = args?.Where(a => a.IndexOf("--gcc", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        var clang = args?.Where(a => a.IndexOf("--clang", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        var nopp = args?.Where(a => a.IndexOf("--nopp", StringComparison.OrdinalIgnoreCase) >= 0).Any() ?? false;
        if (!(vsc || gcc || clang))
            gcc = true;

        // Extract preprocessor options (-D and -I)
        var ppOptions = ExtractPreprocessorOptions(args);

        // Replace input with preprocessed input.
        var source_name = (i.SourceName.EndsWith(".c")) ? i.SourceName : "stdin.c";
        var output_name = source_name + ".p";
        var x1 = i.ToString();
        if (nopp)
        {
            File.WriteAllText(output_name, x1);
            return CharStreams.fromString(x1);
        }
        if (source_name == "stdin.c") File.WriteAllText(source_name, x1);

        if (gcc)
        {
            var psi = new ProcessStartInfo
            {
                FileName = (is_windows ? "gcc.exe":"gcc"),
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };
            psi.ArgumentList.Add("-std=c2x");
            psi.ArgumentList.Add("-E");
            psi.ArgumentList.Add("-C");
            // Add preprocessor options (-D and -I)
            foreach (var opt in ppOptions)
            {
                psi.ArgumentList.Add(opt);
            }
            psi.ArgumentList.Add(source_name);
            string? oldPath = psi.EnvironmentVariables["PATH"]; // inherits from parent
            using (var process = new Process { StartInfo = psi })
            {
                process.Start();

                process.StandardInput.Close(); // signal EOF

                string output = process.StandardOutput.ReadToEnd();
                string error = process.StandardError.ReadToEnd();

                process.WaitForExit();

                File.WriteAllText(output_name, output);
                return CharStreams.fromString(output);
            }
        }
        if (vsc) {
            var clPath = VsWhereJson.FindLatestClExe().ToString();
            if (File.Exists(clPath))
            {
                var psi = new ProcessStartInfo
                {
                    FileName = clPath,
                    RedirectStandardInput = true,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false
                };
                psi.ArgumentList.Add("/E");
                // Add preprocessor options (-D and -I, converted to /D and /I for cl.exe)
                foreach (var opt in ppOptions)
                {
                    if (opt.StartsWith("-D"))
                        psi.ArgumentList.Add("/D" + opt.Substring(2));
                    else if (opt.StartsWith("-I"))
                        psi.ArgumentList.Add("/I" + opt.Substring(2));
                    else
                        psi.ArgumentList.Add(opt);
                }
                psi.ArgumentList.Add(source_name);
                string? oldPath = psi.EnvironmentVariables["PATH"]; // inherits from parent
                using (var process = new Process { StartInfo = psi })
                {
                    process.Start();

                    // If you want to feed GCC input from a file:
                    process.StandardInput.Close(); // signal EOF

                    string output = process.StandardOutput.ReadToEnd();
                    string error = process.StandardError.ReadToEnd();

                    process.WaitForExit();

                    // Set up preprocessor output file and open new charstream
                    // for Antlr.
                    File.WriteAllText(output_name, output);

                    //Console.WriteLine("OUTPUT:\n" + output);
                    //Console.WriteLine("ERROR:\n" + error);
                    return CharStreams.fromString(output);
                }
            }
        }
        if (clang)
        {
            var psi = new ProcessStartInfo
            {
                FileName = (is_windows ? "clang.exe" : "clang"),
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };
            psi.ArgumentList.Add("-std=c2x");
            psi.ArgumentList.Add("-E");
            psi.ArgumentList.Add("-C");
            // Add preprocessor options (-D and -I)
            foreach (var opt in ppOptions)
            {
                psi.ArgumentList.Add(opt);
            }
            psi.ArgumentList.Add(source_name);
            string? oldPath = psi.EnvironmentVariables["PATH"]; // inherits from parent
            using (var process = new Process { StartInfo = psi })
            {
                process.Start();

                process.StandardInput.Close(); // signal EOF

                string output = process.StandardOutput.ReadToEnd();
                string error = process.StandardError.ReadToEnd();

                process.WaitForExit();

                File.WriteAllText(output_name, output);
                return CharStreams.fromString(output);
            }
        }
        throw new Exception("No preprocessor specified.");
    }

    private static List<string> ExtractPreprocessorOptions(List<string> args)
    {
        var options = new List<string>();
        if (args == null) return options;

        foreach (var arg in args)
        {
            // Match --D and --I options (with or without space before value)
            if (arg.StartsWith("--D"))
                options.Add("-D" + arg.Substring(3));
            else if (arg.StartsWith("--I"))
                options.Add("-I" + arg.Substring(3));
        }
        return options;
    }
}
