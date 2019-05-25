const antlr4 = require("antlr4/index");
const TJSLexer = require("./TJSLexer");

function TJSBaseLexer(input) {
    antlr4.Lexer.call(this, input);

    this.lastToken = null;
}

TJSBaseLexer.prototype = Object.create(antlr4.Lexer.prototype);

TJSBaseLexer.prototype.getCurrentToken = function () {
    return antlr4.Lexer.prototype.nextToken.call(this);
};

TJSBaseLexer.prototype.nextToken = function () {
    var next = antlr4.Lexer.prototype.nextToken.call(this);

    if (next.channel === antlr4.Token.DEFAULT_CHANNEL) {
        this.lastToken = next;
    }
    return next;
};

TJSBaseLexer.prototype.IsRegexPossible = function () {
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
};

module.exports.TJSBaseLexer = TJSBaseLexer;