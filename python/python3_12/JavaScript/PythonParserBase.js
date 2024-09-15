import antlr4 from "antlr4";

export default class PythonParserBase extends antlr4.Parser {
    constructor(input) {
        super(input);
    }

    isEqualToCurrentTokenText(tokenText) {
        return this.getCurrentToken().text === tokenText;
    }

    isnotEqualToCurrentTokenText(tokenText) {
        return !this.isEqualToCurrentTokenText(tokenText); // for compatibility with the Python 'not' logical operator
    }
}
