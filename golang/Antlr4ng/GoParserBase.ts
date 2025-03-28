import { Parser, TokenStream, BufferedTokenStream, Token } from 'antlr4ng';
import { GoLexer } from './GoLexer.js';

export default abstract class GoParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    protected closingBracket(): boolean {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LA(1);
        return la === GoLexer.R_CURLY || la === GoLexer.R_PAREN || la === Token.EOF;
    }

    protected isType(): boolean {
        const stream = this.inputStream as BufferedTokenStream;
        const la = stream.LA(1);
        return la !== GoLexer.IDENTIFIER;
    }
}
