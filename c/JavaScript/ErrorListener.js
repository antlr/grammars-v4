import antlr4 from 'antlr4';
import CLexer from './CLexer.js';
import * as fs from 'fs';

export class ErrorListener extends antlr4.error.ErrorListener {
    constructor(quiet, tee, out) {
        super();
        this.had_error = false;
        this._quiet = quiet;
        this._tee = tee;
        this._out = out;
    }

    syntaxError(recognizer, offendingSymbol, line, column, msg, e) {
        let fileName = "<unknown>";
        let lineAdjusted = line;

        if (recognizer instanceof antlr4.Parser) {
            const ts2 = recognizer._input;
            const ind = offendingSymbol.tokenIndex;
            for (let j = ind; j >= 0; j--) {
                const t = ts2.get(j);
                if (t === null) break;
                if (t.type === CLexer.LineDirective) {
                    const txt = t.text;
                    const parts = txt.split(/\s+/);
                    if (parts.length >= 3) {
                        const dirLine = parseInt(parts[1], 10);
                        if (!isNaN(dirLine)) {
                            const lineDirective = t.line;
                            const lineDiff = line - lineDirective;
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
            fs.writeSync(this._out, fileName + " line " + lineAdjusted + ", .p " + line + ":" + column + " " + msg + "\n");
        }
        if (!this._quiet) {
            console.error(fileName + " line " + lineAdjusted + ", .p " + line + ":" + column + " " + msg);
        }
    }
}

export default ErrorListener;
