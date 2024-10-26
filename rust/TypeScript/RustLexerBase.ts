import { CharStream, Token, Lexer } from 'antlr4';
import RustLexer from './RustLexer';

// Abstract class equivalent to RustLexerBase in Java
export default abstract class RustLexerBase extends Lexer {
    current: Token | null = null;
    previous: Token | null = null;

    constructor(input: CharStream) {
        super(input);
    }

    // Override the nextToken function to track the previous and current tokens
    nextToken(): Token {
        const next = super.nextToken();

        if (next.channel === Token.DEFAULT_CHANNEL) {
            this.previous = this.current;
            this.current = next;
        }

        return next;
    }

    SOF(): boolean {
        return this._input.LA(-1) <= 0;
    }

    // Check if the next character matches the expected character
    next(expect: string): boolean {
        return this._input.LA(1) === expect.charCodeAt(0);
    }
    nexti(expect: number): boolean {
        return this._input.LA(1) === input;
    }


    // Determine if a float dot is possible based on the next character
    floatDotPossible(): boolean {
        const next = this._input.LA(1);

        // only block . _ identifier after float
        if (next === '.'.charCodeAt(0) || next === '_'.charCodeAt(0)) {
            return false;
        }
        if (next === 'f'.charCodeAt(0)) {
            // 1.f32
            if (this._input.LA(2) === '3'.charCodeAt(0) && this._input.LA(3) === '2'.charCodeAt(0)) return true;
            // 1.f64
            if (this._input.LA(2) === '6'.charCodeAt(0) && this._input.LA(3) === '4'.charCodeAt(0)) return true;
            return false;
        }
        if ((next >= 'a'.charCodeAt(0) && next <= 'z'.charCodeAt(0)) ||
            (next >= 'A'.charCodeAt(0) && next <= 'Z'.charCodeAt(0))) {
            return false;
        }
        return true;
    }

    // Determine if a float literal is possible based on the previous tokens
    floatLiteralPossible(): boolean {
        if (this.current === null || this.previous === null) return true;
        if (this.current.type !== RustLexer.DOT) return true;

        switch (this.previous.type) {
            case RustLexer.CHAR_LITERAL:
            case RustLexer.STRING_LITERAL:
            case RustLexer.RAW_STRING_LITERAL:
            case RustLexer.BYTE_LITERAL:
            case RustLexer.BYTE_STRING_LITERAL:
            case RustLexer.RAW_BYTE_STRING_LITERAL:
            case RustLexer.INTEGER_LITERAL:
            case RustLexer.DEC_LITERAL:
            case RustLexer.HEX_LITERAL:
            case RustLexer.OCT_LITERAL:
            case RustLexer.BIN_LITERAL:
            case RustLexer.KW_SUPER:
            case RustLexer.KW_SELFVALUE:
            case RustLexer.KW_SELFTYPE:
            case RustLexer.KW_CRATE:
            case RustLexer.KW_DOLLARCRATE:
            case RustLexer.GT:
            case RustLexer.RCURLYBRACE:
            case RustLexer.RSQUAREBRACKET:
            case RustLexer.RPAREN:
            case RustLexer.KW_AWAIT:
            case RustLexer.NON_KEYWORD_IDENTIFIER:
            case RustLexer.RAW_IDENTIFIER:
            case RustLexer.KW_MACRORULES:
                return false;
            default:
                return true;
        }
    }
}
