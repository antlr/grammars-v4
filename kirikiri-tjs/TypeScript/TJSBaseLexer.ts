import { Lexer, CommonTokenStream } from "antlr4ts";
import { threadId } from "worker_threads";
import { Token } from "antlr4";

const antlr4 = require("antlr4/index");
const TJSLexer = require("./TJSLexer");


export abstract class TJSBaseLexer extends Lexer {
    lastToken: Token | null = null;
    constructor(input: CommonTokenStream) {
        super(input);
    }
    getCurrentToken() {
        return this.nextToken();
    }
    nextToken() {
        var next = antlr4.Lexer.prototype.nextToken.call(this);

        if (next.channel === antlr4.Token.DEFAULT_CHANNEL) {
            this.lastToken = next;
        }
        return next;
    }
    IsRegexPossible() {
        if (this.lastToken === null) {
            return true;
        }

        switch (this.lastToken.type) {
            case TJSLexer.Identifier:
            case TJSLexer.NullLiteral:
            case TJSLexer.BooleanLiteral:
            case TJSLexer.This:
            case TJSLexer.CloseBracket:
            case TJSLexer.CloseParen:
            case TJSLexer.OctalIntegerLiteral:
            case TJSLexer.DecimalLiteral:
            case TJSLexer.HexIntegerLiteral:
            case TJSLexer.StringLiteral:
            case TJSLexer.PlusPlus:
            case TJSLexer.MinusMinus:
                return false;
            default:
                return true;
        }
    }
}
