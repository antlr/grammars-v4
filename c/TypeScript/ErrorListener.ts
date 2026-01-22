import { ErrorListener as BaseErrorListener } from "antlr4";
import { Recognizer } from "antlr4";
import { RecognitionException } from "antlr4";
import { Token } from "antlr4";
import { Parser } from "antlr4";
import { CommonTokenStream } from "antlr4";
import CLexer from "./CLexer.js";

export class ErrorListener<T> extends BaseErrorListener<T> {
    public had_error: boolean = false;
    private _quiet: boolean;
    private _tee: boolean;
    private _out: NodeJS.WriteStream | null;

    constructor(quiet: boolean, tee: boolean, out: any) {
        super();
        this._quiet = quiet;
        this._tee = tee;
        this._out = out;
    }

    override syntaxError(
        recognizer: Recognizer<T>,
        offendingSymbol: T,
        line: number,
        column: number,
        msg: string,
        e: RecognitionException | null
    ): void {
        let fileName = "<unknown>";
        let lineAdjusted = line;

        // Get token stream.
        if (recognizer instanceof Parser) {
            const p = recognizer as Parser;
            const q = offendingSymbol as Token;
            const ts2 = p._input as CommonTokenStream;
            // Search back from offending symbol index to find last LineDirective.
            const ind = q.tokenIndex;
            for (let j = ind; j >= 0; j--) {
                const t = ts2.get(j);
                if (t === null) break;
                if (t.type === CLexer.LineDirective) {
                    // Found it.
                    const txt = t.text!;
                    const parts = txt.split(/\s+/);
                    if (parts.length >= 3) {
                        // Get line number from directive.
                        const dirLine = parseInt(parts[1], 10);
                        if (!isNaN(dirLine)) {
                            // Get line number of directive.
                            const lineDirective = t.line;
                            // Get line difference from line directive.
                            const lineDiff = line - lineDirective;
                            // Adjust line number.
                            lineAdjusted = lineDiff + dirLine - 1;
                            fileName = parts[2].trim();
                        }
                    }
                    break;
                }
            }
        }
        this.had_error = true;
        if (this._tee && this._out !== null) {
            this._out.write(fileName + " line " + lineAdjusted + ", .p " + line + ":" + column + " " + msg + "\n");
        }
        if (!this._quiet) {
            console.error(fileName + " line " + lineAdjusted + ", .p " + line + ":" + column + " " + msg);
        }
    }
}
