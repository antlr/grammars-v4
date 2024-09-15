import { Parser, TokenStream } from "antlr4";
//import antlr4 from "antlr4";

export default class PythonParserBase extends Parser {
    constructor(input: TokenStream) {
        super(input);
    }

    isEqualToCurrentTokenText(tokenText: string): boolean {
        return this.getCurrentToken().text === tokenText;
    }

    isnotEqualToCurrentTokenText(tokenText: string): boolean {
        return !this.isEqualToCurrentTokenText(tokenText); // for compatibility with the Python 'not' logical operator
    }
}
