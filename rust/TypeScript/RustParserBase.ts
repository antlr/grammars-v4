import { Parser, TokenStream } from 'antlr4';

export default abstract class RustParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    next(expect: string): boolean {
        return this._input.LA(1) === expect.charCodeAt(0);
    }
}
