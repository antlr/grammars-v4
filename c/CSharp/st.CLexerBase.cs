using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
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
        // Replace input with preprocessed input.

        var source_name = (i.SourceName.EndsWith(".c")) ? i.SourceName : "stdin.c";
        var output_name = source_name + ".p";
        var x1 = i.ToString();
        File.WriteAllText("stdin.c", x1);

    <if(os_win)>
        var clPath = VsWhereJson.FindLatestClExe().ToString();
        if (File.Exists(clPath))
        {
            var psi = new ProcessStartInfo
            {
                FileName = clPath,
                ArgumentList =
                {
                    "/E",
                    source_name,
                },
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };
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
    <endif>
        {
            var psi = new ProcessStartInfo
            {
                FileName = "<if(os_win)>gcc.exe<else>gcc<endif>",
                ArgumentList =
                {
                    "-std=c2x",
                    "-E",
                    "-C",
                    source_name,
                },
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };
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
    }
}
