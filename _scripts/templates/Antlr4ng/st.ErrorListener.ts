import { BaseErrorListener, Recognizer, RecognitionException, Token, Parser, CommonTokenStream, ATNSimulator } from "antlr4ng";
import { writeSync } from "fs";

export class ErrorListener extends BaseErrorListener {
    public had_error: boolean = false;
    private _quiet: boolean;
    private _tee: boolean;
    private _out: number;  // file descriptor from openSync()

    constructor(quiet: boolean, tee: boolean, out: number) {
        super();
        this._quiet = quiet;
        this._tee = tee;
        this._out = out;
    }

    public override syntaxError\<S extends Token, T extends ATNSimulator>(
        recognizer: Recognizer\<T>,
        offendingSymbol: S | null,
        line: number,
        column: number,
        msg: string,
        e: RecognitionException | null
    ): void {
        this.had_error = true;
        if (this._tee && this._out !== null) {
            writeSync(this._out, "line " + line + ":" + column + " " + msg + "\n");
        }
        if (!this._quiet) {
            console.error("line " + line + ":" + column + " " + msg);
        }
    }
}
