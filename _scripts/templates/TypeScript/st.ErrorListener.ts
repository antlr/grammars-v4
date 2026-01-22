import { ErrorListener as BaseErrorListener } from "antlr4";
import { Recognizer } from "antlr4";
import { RecognitionException } from "antlr4";
import { Token } from "antlr4";
import { Parser } from "antlr4";
import { CommonTokenStream } from "antlr4";
import { readFileSync } from 'fs';
import { writeFileSync } from 'fs';
import { openSync } from 'fs';
import { readSync } from 'fs';
import { writeSync } from 'fs';
import { closeSync } from 'fs';
import { readFile } from 'fs/promises'

export class ErrorListener\<T> extends BaseErrorListener\<T> {
    _quiet: boolean;
    _tee: boolean;
    _output: any;
    had_error: boolean;

    constructor(quiet: boolean, tee: boolean, output: any) {
        super();
        this._quiet = quiet;
        this._tee = tee;
        this._output = output;
        this.had_error = false;
    }

    syntaxError(recognizer: Recognizer\<T>, offendingSymbol: T, line: number, column: number, msg: string, e: RecognitionException | undefined): void {
        this.had_error = true;
        if (this._tee) {
            writeSync(this._output, `line ${line}:${column} ${msg}\n`);
        }
        if (!this._quiet) {
            console.error(`line ${line}:${column} ${msg}`);
        }
    }
}

