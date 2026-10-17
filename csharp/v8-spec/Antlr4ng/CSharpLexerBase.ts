import { CharStream, Lexer, Token, CommonToken } from "antlr4ng";
import { CSharpLexer } from "./CSharpLexer.js";

export abstract class CSharpLexerBase extends Lexer {
    // Preprocessor state
    private _pending: Token[] = [];
    private _symbols: Set<string> = new Set<string>();
    private _condition: boolean[] = [];
    private _taken: boolean[] = [];

    // Expression evaluator cursor
    private _expr: Token[] = [];
    private _epos: number = 0;

    constructor(input: CharStream) {
        super(input);
        this._initPreprocessor();
    }

    // -------------------------------------------------------------------------
    // Mode-stack helpers
    // -------------------------------------------------------------------------

    PeekMode(): number {
        const stack = this.modeStack;
        return stack.length > 0 ? stack[stack.length - 1] : Lexer.DEFAULT_MODE;
    }

    override popMode(): number {
        if (this.modeStack.length === 0) {
            process.stderr.write("unbalanced ()/{}/[]\n");
            return Lexer.DEFAULT_MODE;
        }
        return super.popMode();
    }

    PeekModeIs(mode: number): boolean {
        return this.PeekMode() === mode;
    }

    LookAheadIs(pos: number, value: number): boolean {
        return this.inputStream.LA(pos) === value;
    }

    LookAheadIsNot(pos: number, value: number): boolean {
        return this.inputStream.LA(pos) !== value;
    }

    LookAheadIsRBrace1(): boolean    { return this.inputStream.LA(1) === 125; }
    LookAheadIsNotLBrace2(): boolean { return this.inputStream.LA(2) !== 123; }
    PeekModeIsIrsCont(): boolean     { return this.PeekModeIs(CSharpLexer.IRS_CONT); }
    PeekModeIsIvsCont(): boolean     { return this.PeekModeIs(CSharpLexer.IVS_CONT); }

    WrapToken(): void {
        this.text = "\u3014" + (this.text ?? "").replace(/\u3015/g, "\u3015\u3015") + "\u3015";
    }

    // -------------------------------------------------------------------------
    // Preprocessor initialisation
    // -------------------------------------------------------------------------
    private _initPreprocessor(): void {
        for (const arg of process.argv) {
            if (arg.startsWith("--D")) {
                for (const sym of arg.substring(3).split(';')) {
                    if (sym.length > 0) this._symbols.add(sym);
                }
            }
        }
    }

    private _isActive(): boolean {
        return this._condition.length === 0
            || this._condition[this._condition.length - 1];
    }

    // -------------------------------------------------------------------------
    // nextToken override — intercepts DIRECTIVE-channel tokens
    // -------------------------------------------------------------------------
    override nextToken(): Token {
        if (this._pending.length > 0) return this._pending.shift()!;

        const tok = super.nextToken();

        if (tok.channel === 2 /* DIRECTIVE channel */) {
            let skipped: Token | null = null;
            switch (tok.type) {
                case CSharpLexer.DEFINE:   this._handleDefine(); break;
                case CSharpLexer.UNDEF:    this._handleUndef();  break;
                case CSharpLexer.KW_IF:    skipped = this._handleIf();   break;
                case CSharpLexer.ELIF:     skipped = this._handleElif(); break;
                case CSharpLexer.KW_ELSE:  skipped = this._handleElse(); break;
                case CSharpLexer.ENDIF:    this._handleEndif();  break;
            }
            if (skipped !== null) this._pending.push(skipped);
        }

        return tok;
    }

    // -------------------------------------------------------------------------
    // Directive handlers
    // -------------------------------------------------------------------------
    private _handleDefine(): void {
        const line = this._collectLine();
        const sym = this._symbolFromLine(line);
        if (this._isActive() && sym !== null) this._symbols.add(sym);
    }

    private _handleUndef(): void {
        const line = this._collectLine();
        const sym = this._symbolFromLine(line);
        if (this._isActive() && sym !== null) this._symbols.delete(sym);
    }

    private _handleIf(): Token | null {
        const line = this._collectLine();
        const outer = this._isActive();
        const result = outer && this._evaluate(line);
        this._condition.push(result);
        this._taken.push(result);
        return result ? null : this._skipFalseBlock();
    }

    private _handleElif(): Token | null {
        const line = this._collectLine();
        const alreadyTaken = this._taken.length > 0 ? this._taken.pop()! : false;
        if (this._condition.length > 0) this._condition.pop();
        const outer = this._isActive();
        const result = !alreadyTaken && outer && this._evaluate(line);
        this._condition.push(result);
        this._taken.push(alreadyTaken || result);
        return result ? null : this._skipFalseBlock();
    }

    private _handleElse(): Token | null {
        this._collectLine();
        const alreadyTaken = this._taken.length > 0 ? this._taken.pop()! : false;
        if (this._condition.length > 0) this._condition.pop();
        const outer = this._isActive();
        const result = !alreadyTaken && outer;
        this._condition.push(result);
        this._taken.push(true);
        return result ? null : this._skipFalseBlock();
    }

    private _handleEndif(): void {
        this._collectLine();
        if (this._condition.length > 0) this._condition.pop();
        if (this._taken.length > 0)    this._taken.pop();
    }

    // -------------------------------------------------------------------------
    // _collectLine — drain DIRECTIVE_MODE tokens up to DIRECTIVE_NEW_LINE
    // -------------------------------------------------------------------------
    private _collectLine(): Token[] {
        const tokens: Token[] = [];
        let t: Token;
        do {
            t = super.nextToken();
            if (t.channel !== Lexer.HIDDEN)
                tokens.push(t);
        } while (t.type !== CSharpLexer.DIRECTIVE_NEW_LINE && t.type !== Token.EOF);
        return tokens;
    }

    private _symbolFromLine(line: Token[]): string | null {
        for (const t of line)
            if (t.type === CSharpLexer.CONDITIONAL_SYMBOL) return t.text ?? null;
        return null;
    }

    // -------------------------------------------------------------------------
    // _skipFalseBlock — scan char stream, return SKIPPED_SECTION on HIDDEN channel
    // -------------------------------------------------------------------------
    private _skipFalseBlock(): Token {
        let text = "";
        const stream = this.inputStream;
        let depth = 1;
        let atLineStart = true;
        const startLine = this.line;

        while (true) {
            const c = stream.LA(1);
            if (c === Token.EOF) break;

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

        const tok = CommonToken.fromType(CSharpLexer.SKIPPED_SECTION, text);
        tok.channel = Lexer.HIDDEN;
        tok.line = startLine;
        return tok;
    }

    private _peekKeyword(): string {
        let i = 2; // LA(1) is '#'
        while (this.inputStream.LA(i) === 0x20 || this.inputStream.LA(i) === 0x09) i++;
        let kw = "";
        let c: number;
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
    private _evaluate(tokens: Token[]): boolean {
        this._expr = tokens;
        this._epos = 0;
        return this._parseOr();
    }

    private _peekType(): number {
        if (this._epos < this._expr.length) {
            const t = this._expr[this._epos].type;
            if (t !== CSharpLexer.DIRECTIVE_NEW_LINE && t !== Token.EOF) return t;
        }
        return -1;
    }

    private _eConsume(): Token { return this._expr[this._epos++]; }

    private _parseOr(): boolean {
        let v = this._parseAnd();
        while (this._peekType() === CSharpLexer.TK_OR_OR) { this._eConsume(); v = this._parseAnd() || v; }
        return v;
    }

    private _parseAnd(): boolean {
        let v = this._parseEq();
        while (this._peekType() === CSharpLexer.TK_AND_AND) { this._eConsume(); v = this._parseEq() && v; }
        return v;
    }

    private _parseEq(): boolean {
        const v = this._parseUnary();
        if (this._peekType() === CSharpLexer.TK_EQ_EQ) { this._eConsume(); return v === this._parseUnary(); }
        if (this._peekType() === CSharpLexer.TK_NOT_EQ) { this._eConsume(); return v !== this._parseUnary(); }
        return v;
    }

    private _parseUnary(): boolean {
        if (this._peekType() === CSharpLexer.TK_NOT) { this._eConsume(); return !this._parseUnary(); }
        return this._parsePrimary();
    }

    private _parsePrimary(): boolean {
        const t = this._peekType();
        if (t === CSharpLexer.TRUE)               { this._eConsume(); return true; }
        if (t === CSharpLexer.FALSE)              { this._eConsume(); return false; }
        if (t === CSharpLexer.CONDITIONAL_SYMBOL) { return this._symbols.has(this._eConsume().text ?? ""); }
        if (t === CSharpLexer.TK_LPAREN) {
            this._eConsume();
            const v = this._parseOr();
            if (this._peekType() === CSharpLexer.TK_RPAREN) this._eConsume();
            return v;
        }
        return false;
    }
}
