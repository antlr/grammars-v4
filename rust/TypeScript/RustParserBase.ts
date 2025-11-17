import { Parser, TokenStream } from 'antlr4';

export default abstract class RustParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    NextGT(): boolean {
        return this._input.LA(1) === '>'.charCodeAt(0);
    }

    NextLT(): boolean {
        return this._input.LA(1) === '<'.charCodeAt(0);
    }
}
