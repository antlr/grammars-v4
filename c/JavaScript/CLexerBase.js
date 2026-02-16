import antlr4 from 'antlr4';
import { execSync } from 'child_process';
import * as fs from 'fs';

export default class CLexerBase extends antlr4.Lexer {
    constructor(input) {
        super(CLexerBase.runGccAndMakeStream(input));
    }

    static runGccAndMakeStream(input) {
        const isWindows = process.platform === "win32";

        const args = process.argv;

        const vsc = args.some(a => a.toLowerCase().includes("--vsc"));
        let gcc = args.some(a => a.toLowerCase().includes("--gcc"));
        const clang = args.some(a => a.toLowerCase().includes("--clang"));
        const nopp = args.some(a => a.toLowerCase().includes("--nopp"));

        if (!(vsc || gcc || clang)) {
            gcc = true;
        }

        const ppOptions = CLexerBase.extractPreprocessorOptions(args);

        let sourceName = input.name;
        const inputText = input.getText(0, input.size - 1);

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
            return antlr4.CharStreams.fromString(inputText);
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
                // Ignore
            }
            fs.writeFileSync(outputName, output);
            return antlr4.CharStreams.fromString(output);
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
                // Ignore
            }
            fs.writeFileSync(outputName, output);
            return antlr4.CharStreams.fromString(output);
        }

        throw new Error("No preprocessor specified.");
    }

    static extractPreprocessorOptions(args) {
        const options = [];
        for (const arg of args) {
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
