import { Parser, TokenStream, BufferedTokenStream } from 'antlr4';
import GoLexer from './GoLexer';

export default abstract class GoParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    protected closingBracket(): boolean {
        const stream = this._input as BufferedTokenStream;
        const nextTokenType = stream.LA(1);
        return nextTokenType === GoLexer.R_CURLY || nextTokenType === GoLexer.R_PAREN;
    }

    protected isType(): boolean {
        const stream = this._input as BufferedTokenStream;
        const nextTokenType = stream.LA(1);
        return nextTokenType !== GoLexer.IDENTIFIER;
    }
}
