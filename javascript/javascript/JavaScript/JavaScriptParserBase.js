const antlr4 = require("antlr4/index");
const JavaScriptParser = require("./JavaScriptParser");

function JavaScriptParserBase(input) {
    antlr4.Parser.call(this, input);
}

JavaScriptParserBase.prototype = Object.create(antlr4.Parser.prototype);

JavaScriptParserBase.prototype.p = function(str) {
    return this.prev(str);
};

JavaScriptParserBase.prototype.prev = function(str) {
    const source = this._input.LT(-1).source[1].strdata;
    const start = this._input.LT(-1).start;
    const stop = this._input.LT(-1).stop;
    const prev = source.slice(start, stop + 1);
    return prev === str;
};

JavaScriptParserBase.prototype.notLineTerminator = function() {
    return !this.here(JavaScriptParser.LineTerminator);
};

JavaScriptParserBase.prototype.notOpenBraceAndNotFunction = function() {
    const nextTokenType = this._input.LT(1).type;
    return (
        nextTokenType !== JavaScriptParser.OpenBrace &&
        nextTokenType !== JavaScriptParser.Function
    );
};

JavaScriptParserBase.prototype.closeBrace = function() {
    return this._input.LT(1).type === JavaScriptParser.CloseBrace;
};

JavaScriptParserBase.prototype.here = function(type) {
    const possibleIndexEosToken = this.getCurrentToken().tokenIndex - 1;
    const ahead = this._input.get(possibleIndexEosToken);
    return ahead.channel === antlr4.Lexer.HIDDEN && ahead.type === type;
};

JavaScriptParserBase.prototype.lineTerminatorAhead = function() {
    let possibleIndexEosToken = this.getCurrentToken().tokenIndex - 1;
    let ahead = this._input.get(possibleIndexEosToken);
    if (ahead.channel !== antlr4.Lexer.HIDDEN) {
        return false;
    }

    if (ahead.type === JavaScriptParser.LineTerminator) {
        return true;
    }

    if (ahead.type === JavaScriptParser.WhiteSpaces) {
        possibleIndexEosToken = this.getCurrentToken().getTokenIndex() - 2;
        ahead = this._input.get(possibleIndexEosToken);
    }

    const text = ahead.type;
    const type = ahead.type;

    return (
        (type === JavaScriptParser.MultiLineComment &&
            (text.includes("\r") || text.includes("\n"))) ||
        type === JavaScriptParser.LineTerminator
    );
};

module.exports.JavaScriptParserBase = JavaScriptParserBase;
