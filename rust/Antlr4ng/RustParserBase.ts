import { Parser, TokenStream } from 'antlr4ng';
import { RustParser } from './RustParser.js';

export default abstract class RustParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    NextGT(): boolean {
        return this.inputStream.LA(1) === RustParser.GT;
    }

    NextLT(): boolean {
        return this.inputStream.LA(1) === RustParser.LT;
    }
}
