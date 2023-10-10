import { Lexer } from "antlr4";
export default class BisonLexerBase extends Lexer {
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
        while (cs.LA(1) !== 12 /* '\n' */ && cs.LA(1) !== -1) {
            cs.consume();
        }
    }
    read_long_string(cs, sep) {
        let done = false;
        cs.consume();
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
                        cs.consume();
                        done = true;
                    }
                    break;
                default:
                    if (cs.LA(1) === -1) {
                        done = true;
                        break;
                    }
                    cs.consume();
                    break;
            }
            if (done)
                break;
        }
    }
    skip_sep(cs) {
        let count = 0;
        let s = cs.LA(1);
        cs.consume();
        while (cs.LA(1) === 61 /* '=' */) {
            cs.consume();
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
