const antlr4 = require("antlr4/index");
const TJSParser = require("./TJSParser");

function TJSBaseParser(input) {
    antlr4.Parser.call(this, input);
}

TJSBaseParser.prototype = Object.create(antlr4.Parser.prototype);

TJSBaseParser.prototype.notOpenBraceAndNotFunction = function () {
    const nextTokenType = this._input.LT(1).type;
    return (
        nextTokenType !== TJSParser.OpenBrace &&
        nextTokenType !== TJSParser.Function
    );
};

TJSBaseParser.prototype.closeBrace = function () {
    return this._input.LT(1).type === TJSParser.CloseBrace;
};

TJSBaseParser.prototype.lineTerminatorAhead = function () {
    let possibleIndexEosToken = this.getCurrentToken().tokenIndex - 1;
    let ahead = this._input.get(possibleIndexEosToken);
    if (ahead.channel !== antlr4.Lexer.HIDDEN) {
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
};

module.exports.TJSBaseParser = TJSBaseParser;