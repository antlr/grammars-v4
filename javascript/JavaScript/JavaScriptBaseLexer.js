const antlr4 = require("antlr4/index");
const JavaScriptLexer = require("./JavaScriptLexer");

function JavaScriptBaseLexer(input) {
    antlr4.Lexer.call(this, input);

    this.scopeStrictModes = new Array();
    this.lastToken = null;
    this.useStrictDefault = false;
    this.useStrictCurrent = false;
}

JavaScriptBaseLexer.prototype = Object.create(antlr4.Lexer.prototype);

JavaScriptBaseLexer.prototype.getStrictDefault = function() {
    return this.useStrictDefault;
};

JavaScriptBaseLexer.prototype.setUseStrictDefault = function(value) {
    this.useStrictDefault = value;
    this.useStrictCurrent = value;
};

JavaScriptBaseLexer.prototype.IsStrictMode = function() {
    return this.useStrictCurrent;
};

JavaScriptBaseLexer.prototype.getCurrentToken = function() {
    return antlr4.Lexer.prototype.nextToken.call(this);
};

JavaScriptBaseLexer.prototype.nextToken = function() {
    var next = antlr4.Lexer.prototype.nextToken.call(this);

    if (next.channel === antlr4.Token.DEFAULT_CHANNEL) {
        this.lastToken = next;
    }
    return next;
};

JavaScriptBaseLexer.prototype.ProcessOpenBrace = function() {
    this.useStrictCurrent =
        this.scopeStrictModes.length > 0 && this.scopeStrictModes[0]
            ? true
            : this.useStrictDefault;
    this.scopeStrictModes.push(this.useStrictCurrent);
};

JavaScriptBaseLexer.prototype.ProcessCloseBrace = function() {
    this.useStrictCurrent =
        this.scopeStrictModes.length > 0
            ? this.scopeStrictModes.pop()
            : this.useStrictDefault;
};

JavaScriptBaseLexer.prototype.ProcessStringLiteral = function() {
    if (
        this.lastToken !== undefined &&
        (this.lastToken === null ||
            this.lastToken.type === JavaScriptLexer.OpenBrace)
    ) {
        const text = this._input.strdata.slice(0, "use strict".length);
        if (text === '"use strict"' || text === "'use strict'") {
            if (this.scopeStrictModes.length > 0) {
                this.scopeStrictModes.pop();
            }
            this.useStrictCurrent = true;
            this.scopeStrictModes.push(this.useStrictCurrent);
        }
    }
};

JavaScriptBaseLexer.prototype.IsRegexPossible = function() {
    if (this.lastToken === null) {
        return true;
    }

    switch (this.lastToken.type) {
        case JavaScriptLexer.Identifier:
        case JavaScriptLexer.NullLiteral:
        case JavaScriptLexer.BooleanLiteral:
        case JavaScriptLexer.This:
        case JavaScriptLexer.CloseBracket:
        case JavaScriptLexer.CloseParen:
        case JavaScriptLexer.OctalIntegerLiteral:
        case JavaScriptLexer.DecimalLiteral:
        case JavaScriptLexer.HexIntegerLiteral:
        case JavaScriptLexer.StringLiteral:
        case JavaScriptLexer.PlusPlus:
        case JavaScriptLexer.MinusMinus:
            return false;
        default:
            return true;
    }
};

module.exports.JavaScriptBaseLexer = JavaScriptBaseLexer;
