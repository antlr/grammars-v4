import { CharStream, Token, Lexer } from 'antlr4ng';
import { RustLexer } from './RustLexer.js';

// Abstract class equivalent to RustLexerBase in Java
export default abstract class RustLexerBase extends Lexer {
    lt1: Token | null = null;
    lt2: Token | null = null;

    constructor(input: CharStream) {
        super(input);
    }

    // Override the nextToken function to track the previous and current tokens
    nextToken(): Token {
        const next = super.nextToken();

        if (next.channel === Token.DEFAULT_CHANNEL) {
            this.lt2 = this.lt1;
            this.lt1 = next;
        }

        return next;
    }

    SOF(): boolean {
        return this.inputStream.LA(-1) <= 0;
    }

    // Determine if a float dot is possible based on the next character
    FloatDotPossible(): boolean {
        const next = this.inputStream.LA(1);

        // only block . _ identifier after float
        if (next === '.'.charCodeAt(0) || next === '_'.charCodeAt(0)) {
            return false;
        }
        if (next === 'f'.charCodeAt(0)) {
            // 1.f32
            if (this.inputStream.LA(2) === '3'.charCodeAt(0) && this.inputStream.LA(3) === '2'.charCodeAt(0)) return true;
            // 1.f64
            if (this.inputStream.LA(2) === '6'.charCodeAt(0) && this.inputStream.LA(3) === '4'.charCodeAt(0)) return true;
            return false;
        }
        if ((next >= 'a'.charCodeAt(0) && next <= 'z'.charCodeAt(0)) ||
            (next >= 'A'.charCodeAt(0) && next <= 'Z'.charCodeAt(0))) {
            return false;
        }
        return true;
    }

    // Determine if a float literal is possible based on the previous tokens
    FloatLiteralPossible(): boolean {
        if (this.lt1 === null || this.lt2 === null)
	{
		return true;
	}
        if (this.lt1.type !== RustLexer.DOT)
	{
		return true;
	}

        switch (this.lt2.type) {
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