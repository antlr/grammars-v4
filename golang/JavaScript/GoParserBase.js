import { Parser, Token } from 'antlr4';
import GoLexer from './GoLexer.js';

export default class GoParserBase extends Parser {
    constructor(input) {
        super(input);
    }

    closingBracket() {
        const stream = this._input;
        const la = stream.LA(1);
        return la === GoLexer.R_CURLY || la === GoLexer.R_PAREN || la === Token.EOF;
    }

    isType() {
        const stream = this._input;
        const la = stream.LA(1);
        return la !== GoLexer.IDENTIFIER;
    }
}

