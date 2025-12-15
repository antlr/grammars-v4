using Antlr4.Runtime;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;

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

        // Detect which type of input.
        if (i.SourceName.EndsWith(".c"))
        {
            var psi = new ProcessStartInfo
            {
                FileName = "C:\\msys64\\mingw64\\bin\\gcc.exe",
                ArgumentList =
                {
                    "-std=c2x",
					"-E",
					"-C",
                    i.SourceName
                },
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };

            // Prepend the MSYS2/mingw64 bin directory to PATH for this child process.
            string msysBin = @"C:\msys64\mingw64\bin";
            string? oldPath = psi.EnvironmentVariables["PATH"]; // inherits from parent

            if (string.IsNullOrEmpty(oldPath))
            {
                psi.EnvironmentVariables["PATH"] = msysBin;
            }
            else
            {
                psi.EnvironmentVariables["PATH"] = msysBin + ";" + oldPath;
            }

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
                File.WriteAllText(i.SourceName + ".p", output);
                
                //Console.WriteLine("OUTPUT:\n" + output);
                //Console.WriteLine("ERROR:\n" + error);
                return CharStreams.fromString(output);
            }
            
        }
        else
        {
            var psi = new ProcessStartInfo
            {
                FileName = "C:\\msys64\\mingw64\\bin\\gcc.exe",
                ArgumentList =
                {
                    "-std=c2x",
                    "-E",
                    i.SourceName
                },
                RedirectStandardInput = true,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                UseShellExecute = false
            };

            // Prepend the MSYS2/mingw64 bin directory to PATH for this child process.
            string msysBin = @"C:\msys64\mingw64\bin";
            string? oldPath = psi.EnvironmentVariables["PATH"]; // inherits from parent

            if (string.IsNullOrEmpty(oldPath))
            {
                psi.EnvironmentVariables["PATH"] = msysBin;
            }
            else
            {
                psi.EnvironmentVariables["PATH"] = msysBin + ";" + oldPath;
            }

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
                File.WriteAllText("stdin.p", output);

                //Console.WriteLine("OUTPUT:\n" + output);
                //Console.WriteLine("ERROR:\n" + error);
                return CharStreams.fromString(output);
            }
        }
    }
}
