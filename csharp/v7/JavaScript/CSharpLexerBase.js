import antlr4 from 'antlr4';
import CSharpLexer from './CSharpLexer.js';

export default class CSharpLexerBase extends antlr4.Lexer {
    constructor(input) {
        super(input);
        this.interpolatedStringLevel = 0;
        this.interpolatedVerbatiums = [];
        this.curlyLevels = [];
        this.verbatium = false;

        // Preprocessor state
        this._pending = [];
        this._symbols = new Set();
        this._condition = [];
        this._taken = [];

        // Expression evaluator cursor
        this._expr = [];
        this._epos = 0;

        this._initPreprocessor();
    }

    OnInterpolatedRegularStringStart() {
        this.interpolatedStringLevel++;
        this.interpolatedVerbatiums.push(false);
        this.verbatium = false;
    }

    OnInterpolatedVerbatiumStringStart() {
        this.interpolatedStringLevel++;
        this.interpolatedVerbatiums.push(true);
        this.verbatium = true;
    }

    OnOpenBrace() {
        if (this.interpolatedStringLevel > 0) {
            this.curlyLevels[this.curlyLevels.length - 1]++;
        }
    }

    OnCloseBrace() {
        if (this.interpolatedStringLevel > 0) {
            this.curlyLevels[this.curlyLevels.length - 1]--;
            if (this.curlyLevels[this.curlyLevels.length - 1] === 0) {
                this.curlyLevels.pop();
                this.skip();
                this.popMode();
            }
        }
    }

    OnColon() {
        if (this.interpolatedStringLevel > 0) {
            let ind = 1;
            let switchToFormatString = true;
            while (this.inputStream.LA(ind) !== 0x7D /* } */) {
                const ch = this.inputStream.LA(ind);
                if (ch === 0x3A /* : */ || ch === 0x29 /* ) */) {
                    switchToFormatString = false;
                    break;
                }
                ind++;
            }
            if (switchToFormatString) {
                this.setMode(CSharpLexer.INTERPOLATION_FORMAT);
            }
        }
    }

    OpenBraceInside() {
        this.curlyLevels.push(1);
    }

    OnDoubleQuoteInside() {
        this.interpolatedStringLevel--;
        this.interpolatedVerbatiums.pop();
        this.verbatium = this.interpolatedVerbatiums.length > 0
            && this.interpolatedVerbatiums[this.interpolatedVerbatiums.length - 1];
    }

    OnCloseBraceInside() {
        this.curlyLevels.pop();
    }

    IsRegularCharInside() {
        return !this.verbatium;
    }

    IsVerbatiumDoubleQuoteInside() {
        return this.verbatium;
    }

    // -------------------------------------------------------------------------
    // Preprocessor initialisation
    // -------------------------------------------------------------------------
    _initPreprocessor() {
        for (const arg of process.argv) {
            if (arg.startsWith("--D")) {
                for (const sym of arg.substring(3).split(';')) {
                    if (sym.length > 0) this._symbols.add(sym);
                }
            }
        }
    }

    _isActive() {
        return this._condition.length === 0
            || this._condition[this._condition.length - 1];
    }

    // -------------------------------------------------------------------------
    // nextToken override — intercepts DIRECTIVE-channel tokens
    // -------------------------------------------------------------------------
    nextToken() {
        if (this._pending.length > 0) return this._pending.shift();

        const tok = super.nextToken();

        if (tok.channel === CSharpLexer.DIRECTIVE) {
            let skipped = null;
            switch (tok.type) {
                case CSharpLexer.DEFINE: this._handleDefine(); break;
                case CSharpLexer.UNDEF:  this._handleUndef();  break;
                case CSharpLexer.IF:     skipped = this._handleIf();   break;
                case CSharpLexer.ELIF:   skipped = this._handleElif(); break;
                case CSharpLexer.ELSE:   skipped = this._handleElse(); break;
                case CSharpLexer.ENDIF:  this._handleEndif();  break;
            }
            if (skipped !== null) this._pending.push(skipped);
        }

        return tok;
    }

    // -------------------------------------------------------------------------
    // Directive handlers
    // -------------------------------------------------------------------------
    _handleDefine() {
        const line = this._collectLine();
        const sym = this._symbolFromLine(line);
        if (this._isActive() && sym !== null) this._symbols.add(sym);
    }

    _handleUndef() {
        const line = this._collectLine();
        const sym = this._symbolFromLine(line);
        if (this._isActive() && sym !== null) this._symbols.delete(sym);
    }

    _handleIf() {
        const line = this._collectLine();
        const outer = this._isActive();
        const result = outer && this._evaluate(line);
        this._condition.push(result);
        this._taken.push(result);
        return result ? null : this._skipFalseBlock();
    }

    _handleElif() {
        const line = this._collectLine();
        const alreadyTaken = this._taken.length > 0 ? this._taken.pop() : false;
        if (this._condition.length > 0) this._condition.pop();
        const outer = this._isActive();
        const result = !alreadyTaken && outer && this._evaluate(line);
        this._condition.push(result);
        this._taken.push(alreadyTaken || result);
        return result ? null : this._skipFalseBlock();
    }

    _handleElse() {
        this._collectLine();
        const alreadyTaken = this._taken.length > 0 ? this._taken.pop() : false;
        if (this._condition.length > 0) this._condition.pop();
        const outer = this._isActive();
        const result = !alreadyTaken && outer;
        this._condition.push(result);
        this._taken.push(true);
        return result ? null : this._skipFalseBlock();
    }

    _handleEndif() {
        this._collectLine();
        if (this._condition.length > 0) this._condition.pop();
        if (this._taken.length > 0)    this._taken.pop();
    }

    // -------------------------------------------------------------------------
    // _collectLine — drain DIRECTIVE_MODE tokens up to DIRECTIVE_NEW_LINE
    // -------------------------------------------------------------------------
    _collectLine() {
        const tokens = [];
        let t;
        do {
            t = super.nextToken();
            if (t.channel !== antlr4.Lexer.HIDDEN && t.channel !== CSharpLexer.COMMENTS_CHANNEL)
                tokens.push(t);
        } while (t.type !== CSharpLexer.DIRECTIVE_NEW_LINE && t.type !== antlr4.Token.EOF);
        return tokens;
    }

    _symbolFromLine(line) {
        for (const t of line)
            if (t.type === CSharpLexer.CONDITIONAL_SYMBOL) return t.text;
        return null;
    }

    // -------------------------------------------------------------------------
    // _skipFalseBlock — scan char stream, return SKIPPED_SECTION on HIDDEN channel
    // -------------------------------------------------------------------------
    _skipFalseBlock() {
        let text = "";
        const stream = this.inputStream;
        let depth = 1;
        let atLineStart = true;
        const startLine = this.line;

        while (true) {
            const c = stream.LA(1);
            if (c === antlr4.Token.EOF) break;

            if (c === 0x0D || c === 0x0A || c === 0x85 || c === 0x2028 || c === 0x2029) {
                stream.consume();
                text += String.fromCodePoint(c);
                if (c === 0x0D && stream.LA(1) === 0x0A) {
                    stream.consume();
                    text += '\n';
                }
                atLineStart = true;
                continue;
            }

            if (atLineStart && (c === 0x20 || c === 0x09)) {
                stream.consume();
                text += String.fromCodePoint(c);
                continue;
            }

            if (atLineStart && c === 0x23 /* '#' */) {
                const kw = this._peekKeyword();
                if (kw === "if") {
                    depth++;
                } else if (kw === "endif") {
                    if (--depth === 0) break;
                } else if ((kw === "else" || kw === "elif") && depth === 1) {
                    break;
                }
            }

            atLineStart = false;
            stream.consume();
            text += String.fromCodePoint(c);
        }

        const tok = new antlr4.CommonToken([this, this.inputStream], CSharpLexer.SKIPPED_SECTION,
            antlr4.Lexer.HIDDEN, -1, -1);
        tok.text = text;
        tok.line = startLine;
        return tok;
    }

    _peekKeyword() {
        let i = 2; // LA(1) is '#'
        while (this.inputStream.LA(i) === 0x20 || this.inputStream.LA(i) === 0x09) i++;
        let kw = "";
        let c;
        while ((c = this.inputStream.LA(i)) !== -1
                && ((c >= 0x61 && c <= 0x7A) || (c >= 0x41 && c <= 0x5A))) {
            kw += String.fromCodePoint(c);
            i++;
        }
        return kw;
    }

    // -------------------------------------------------------------------------
    // Recursive-descent expression evaluator
    // -------------------------------------------------------------------------
    _evaluate(tokens) {
        this._expr = tokens;
        this._epos = 0;
        return this._parseOr();
    }

    _peekType() {
        if (this._epos < this._expr.length) {
            const t = this._expr[this._epos].type;
            if (t !== CSharpLexer.DIRECTIVE_NEW_LINE && t !== antlr4.Token.EOF) return t;
        }
        return -1;
    }

    _eConsume() { return this._expr[this._epos++]; }

    _parseOr() {
        let v = this._parseAnd();
        while (this._peekType() === CSharpLexer.OP_OR) { this._eConsume(); v = this._parseAnd() || v; }
        return v;
    }

    _parseAnd() {
        let v = this._parseEq();
        while (this._peekType() === CSharpLexer.OP_AND) { this._eConsume(); v = this._parseEq() && v; }
        return v;
    }

    _parseEq() {
        const v = this._parseUnary();
        if (this._peekType() === CSharpLexer.OP_EQ) { this._eConsume(); return v === this._parseUnary(); }
        if (this._peekType() === CSharpLexer.OP_NE) { this._eConsume(); return v !== this._parseUnary(); }
        return v;
    }

    _parseUnary() {
        if (this._peekType() === CSharpLexer.BANG) { this._eConsume(); return !this._parseUnary(); }
        return this._parsePrimary();
    }

    _parsePrimary() {
        const t = this._peekType();
        if (t === CSharpLexer.TRUE)               { this._eConsume(); return true; }
        if (t === CSharpLexer.FALSE)              { this._eConsume(); return false; }
        if (t === CSharpLexer.CONDITIONAL_SYMBOL) { return this._symbols.has(this._eConsume().text ?? ""); }
        if (t === CSharpLexer.OPEN_PARENS) {
            this._eConsume();
            const v = this._parseOr();
            if (this._peekType() === CSharpLexer.CLOSE_PARENS) this._eConsume();
            return v;
        }
        return false;
    }
}
