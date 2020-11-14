import { CommonTokenStream, Parser, Lexer } from "antlr4ts";
//import * as TJSParser from "./TJSParser";
const TJSParser = require("./TJSParser");
export abstract class TJSBaseParser extends Parser {
    constructor(input: CommonTokenStream) {
        super(input)
    }
    notOpenBraceAndNotFunction() {
        const nextTokenType = this._input.LT(1).type;
        return (
            nextTokenType !== TJSParser.OpenBrace &&
            nextTokenType !== TJSParser.Function
        );
    }

    closeBrace() {
        return this._input.LT(1).type === TJSParser.CloseBrace;
    }
    lineTerminatorAhead() {
        let possibleIndexEosToken = this.getCurrentToken().tokenIndex - 1;
        let ahead = this._input.get(possibleIndexEosToken);
        if (ahead.channel !== Lexer.HIDDEN) {
            return false;
        }

        if (ahead.type === TJSParser.LineTerminator) {
            return true;
        }

        if (ahead.type === TJSParser.WhiteSpaces) {
            possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 2;
            ahead = this._input.get(possibleIndexEosToken);
        }

        const text = ahead.type;
        const type = ahead.type;

        return (
            (type === TJSParser.MultiLineComment &&
                (text.includes("\r") || text.includes("\n"))) ||
            type === TJSParser.LineTerminator
        );
    }
}
