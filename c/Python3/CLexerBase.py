import sys
import os
import platform
import subprocess
from antlr4 import Lexer, InputStream

class CLexerBase(Lexer):
    def __init__(self, input, output=sys.stdout):
        super().__init__(CLexerBase.runGccAndMakeStream(input), output)

    @staticmethod
    def runGccAndMakeStream(input):
        is_windows = platform.system() == "Windows"

        args = sys.argv

        vsc = any("--vsc" in a.lower() for a in args)
        gcc = any("--gcc" in a.lower() for a in args)
        clang = any("--clang" in a.lower() for a in args)
        nopp = any("--nopp" in a.lower() for a in args)

        if not (vsc or gcc or clang):
            gcc = True

        ppOptions = CLexerBase.extractPreprocessorOptions(args)

        sourceName = getattr(input, 'name', None) or getattr(input, 'fileName', None) or ""
        inputText = input.getText(0, input.size - 1)

        if not sourceName or not sourceName.endswith(".c"):
            sourceName = "stdin.c"

        outputName = sourceName + ".p"

        if nopp:
            try:
                with open(outputName, "w") as f:
                    f.write(inputText)
            except:
                pass
            return InputStream(inputText)

        if sourceName == "stdin.c":
            with open(sourceName, "w") as f:
                f.write(inputText)

        if gcc:
            output = ""
            try:
                gccCommand = "gcc.exe" if is_windows else "gcc"
                ppOptsStr = " ".join('"' + o + '"' for o in ppOptions)
                cmd = f'{gccCommand} -std=c2x -E -C {ppOptsStr} "{sourceName}"'
                result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
                output = result.stdout
            except:
                pass
            with open(outputName, "w") as f:
                f.write(output)
            return InputStream(output)

        if clang:
            output = ""
            try:
                clangCommand = "clang.exe" if is_windows else "clang"
                ppOptsStr = " ".join('"' + o + '"' for o in ppOptions)
                cmd = f'{clangCommand} -std=c2x -E -C {ppOptsStr} "{sourceName}"'
                result = subprocess.run(cmd, shell=True, capture_output=True, text=True)
                output = result.stdout
            except:
                pass
            with open(outputName, "w") as f:
                f.write(output)
            return InputStream(output)

        raise Exception("No preprocessor specified.")

    @staticmethod
    def extractPreprocessorOptions(args):
        options = []
        for arg in args:
            if arg.startswith("--D"):
                options.append("-D" + arg[3:])
            elif arg.startswith("--I"):
                options.append("-I" + arg[3:])
        return options
