import { Lexer } from "antlr4";
export default class LuaLexerBase extends Lexer {
    constructor(input) {
        super(input);
    }
    HandleComment() {
        this.start_line = this.line;
        this.start_col = this.column - 2;
        let cs = this._input;
        if (cs.LA(1) === 91) { /* '[' */
            let sep = this.skip_sep(cs);
            if (sep >= 2) {
                this.read_long_string(cs, sep);
                return;
            }
        }
        while (cs.LA(1) !== 10 /* '\n' */ && cs.LA(1) !== -1) {
            this._interp.consume(cs);
        }
    }
    read_long_string(cs, sep) {
        let done = false;
        this._interp.consume(cs);
        for (;;) {
            let c = cs.LA(1);
            switch (c) {
                case -1:
                    done = true;
                    //                    let listener = this.getErrorListenerDispatch();
                    //                    listener.syntaxError(this, null, this.start_line, this.start_col, "unfinished long comment", null);
                    break;
                case 93: /* ']' */
                    if (this.skip_sep(cs) === sep) {
                        this._interp.consume(cs);
                        done = true;
                    }
                    break;
                default:
                    if (cs.LA(1) === -1) {
                        done = true;
                        break;
                    }
                    this._interp.consume(cs);
                    break;
            }
            if (done)
                break;
        }
    }
    skip_sep(cs) {
        let count = 0;
        let s = cs.LA(1);
        this._interp.consume(cs);
        while (cs.LA(1) === 61 /* '=' */) {
            this._interp.consume(cs);
            count++;
        }
        if (cs.LA(1) === s)
            count += 2;
        else if (count === 0)
            count = 1;
        else
            count = 0;
        return count;
    }
    IsLine1Col0() {
        let cs = this._input;
        return cs.index === 1;
    }
}
