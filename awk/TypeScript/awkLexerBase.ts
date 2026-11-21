import { CharStream, Lexer, Token } from "antlr4";
import awkLexer from './awkLexer';

export default abstract class awkLexerBase extends Lexer {
    private _afterExpr: boolean = false;

    constructor(input: CharStream) {
        super(input);
    }

    nextToken(): Token {
        const token = super.nextToken();
        if (token.channel === Token.DEFAULT_CHANNEL) {
            this._afterExpr = token.type === awkLexer.WORD
                || token.type === awkLexer.NUMBER
                || token.type === awkLexer.STRING
                || token.type === awkLexer.BUILTIN_FUNC_NAME
                || token.type === awkLexer.INCR
                || token.type === awkLexer.DECR
                || token.type === awkLexer.Rp
                || token.type === awkLexer.Rb;
        }
        return token;
    }

    IsNotAfterExpr(): boolean {
        return !this._afterExpr;
    }
}
