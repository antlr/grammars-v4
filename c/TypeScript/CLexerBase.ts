import { Lexer, CharStream, CharStreams } from "antlr4";
import { execSync } from "child_process";
import * as fs from "fs";
import * as os from "os";
import * as path from "path";

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

        // Get the source name from the CharStream
        let sourceName = "<unknown>"; // (API does not exist) input.getSourceName();
        const inputText = input.getText(0, input.size - 1);

        // If source name is empty or not a .c file, we need to write to a temp file
        if (!sourceName || sourceName === "" || !sourceName.endsWith(".c")) {
            // Create a temp file for preprocessing
            sourceName = path.join(os.tmpdir(), "antlr4ng_temp_" + Date.now() + ".c");
            fs.writeFileSync(sourceName, inputText);
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

        if (gcc) {
            let output = "";
            try {
                const gccCommand = isWindows ? "gcc.exe" : "gcc";
                output = execSync(`${gccCommand} -std=c2x -E -C "${sourceName}"`, {
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
                output = execSync(`${clangCommand} -std=c2x -E -C "${sourceName}"`, {
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
}
