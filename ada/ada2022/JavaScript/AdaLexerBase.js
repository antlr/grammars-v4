import antlr4 from 'antlr4';
import AdaLexer from './AdaLexer.js';

export default class AdaLexerBase extends antlr4.Lexer {
    constructor(input) {
        super(input);
        this._lastTokenType = 0;
    }

    nextToken() {
        var token = super.nextToken();
        if (token.channel === antlr4.Token.DEFAULT_CHANNEL) {
            this._lastTokenType = token.type;
        }
        return token;
    }

    IsCharLiteralAllowed() {
        // In Ada, a tick after an identifier, closing paren, or 'all' keyword
        // is an attribute tick, not the start of a character literal.
        return this._lastTokenType !== AdaLexer.IDENTIFIER_
            && this._lastTokenType !== AdaLexer.RP
            && this._lastTokenType !== AdaLexer.ALL;
    }
}
