import { Parser, TokenStream, BufferedTokenStream } from 'antlr4';
import GoLexer from './GoLexer';

console.log(Parser)

export default abstract class GoParserBase extends Parser {
  constructor(input: TokenStream) {
    super(input);
  }

  protected closingBracket(): boolean {
    const stream = this._input as BufferedTokenStream;
    const prevTokenType = stream.LA(1);

    return prevTokenType === GoLexer.R_CURLY || prevTokenType === GoLexer.R_PAREN;
  }
}
