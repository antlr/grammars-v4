import { Parser, TokenStream, BufferedTokenStream } from 'antlr4';
import GoLexer from './GoLexer';

export default abstract class GoParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    protected closingBracket(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LA(1);
        return la === GoLexer.R_CURLY || la === GoLexer.R_PAREN || la === Token.EOF;
    }

    protected isType(): boolean {
        const stream = this._input as BufferedTokenStream;
        const la = stream.LA(1);
        return la !== GoLexer.IDENTIFIER;
    }
}
