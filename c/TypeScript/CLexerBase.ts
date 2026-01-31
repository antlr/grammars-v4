import { Lexer, CharStream, CharStreams } from "antlr4";
import { execSync } from "child_process";
import * as fs from "fs";

export default abstract class CLexerBase extends Lexer {
    constructor(input: CharStream) {
        super(CLexerBase.runGccAndMakeStream(input));
    }

    public static runGccAndMakeStream(input: CharStream): CharStream {
        const isWindows = process.platform === "win32";

        // Get options from command line args
        const args = process.argv;

        const vsc = args.some(a => a.toLowerCase().includes("--vsc"));
        let gcc = args.some(a => a.toLowerCase().includes("--gcc"));
        const clang = args.some(a => a.toLowerCase().includes("--clang"));
        const nopp = args.some(a => a.toLowerCase().includes("--nopp"));

        if (!(vsc || gcc || clang)) {
            gcc = true;
        }

        // Extract preprocessor options (-D and -I)
        const ppOptions = CLexerBase.extractPreprocessorOptions(args);

        // Get the source name from the CharStream
        let sourceName = (input as any).name;  // (API does not exist) input.getSourceName(); HACK!!!
        const inputText = input.getText(0, input.size - 1);

        // If source name is empty or not a .c file, use stdin.c
        if (!sourceName || sourceName === "" || !sourceName.endsWith(".c")) {
            sourceName = "stdin.c";
        }

        const outputName = sourceName + ".p";

        if (nopp) {
            try {
                fs.writeFileSync(outputName, inputText);
            } catch (e) {
                // Ignore
            }
            return CharStreams.fromString(inputText);
        }

        if (sourceName === "stdin.c") {
            fs.writeFileSync(sourceName, inputText);
        }

        if (gcc) {
            let output = "";
            try {
                const gccCommand = isWindows ? "gcc.exe" : "gcc";
                const ppOptsStr = ppOptions.map(o => `"${o}"`).join(" ");
                output = execSync(`${gccCommand} -std=c2x -E -C ${ppOptsStr} "${sourceName}"`, {
                    encoding: "utf-8",
                    maxBuffer: 50 * 1024 * 1024,
                    stdio: ["ignore", "pipe", "pipe"],
                });
            } catch (e) {
//                throw new Error("Failed to run gcc preprocessor: " + e);
            }
            fs.writeFileSync(outputName, output);
            return CharStreams.fromString(output);
        }
        if (clang) {
            let output = "";
            try {
                const clangCommand = isWindows ? "clang.exe" : "clang";
                const ppOptsStr = ppOptions.map(o => `"${o}"`).join(" ");
                output = execSync(`${clangCommand} -std=c2x -E -C ${ppOptsStr} "${sourceName}"`, {
                    encoding: "utf-8",
                    maxBuffer: 50 * 1024 * 1024,
                    stdio: ["ignore", "pipe", "pipe"],
                });
            } catch (e) {
//                throw new Error("Failed to run clang preprocessor: " + e);
            }
            fs.writeFileSync(outputName, output);
            return CharStreams.fromString(output);
        }

        throw new Error("No preprocessor specified.");
    }

    private static extractPreprocessorOptions(args: string[]): string[] {
        const options: string[] = [];
        for (const arg of args) {
            // Match --D and --I options
            if (arg.startsWith("--D")) {
                options.push("-D" + arg.substring(3));
            }
            else if (arg.startsWith("--I")) {
                options.push("-I" + arg.substring(3));
            }
        }
        return options;
    }
}
