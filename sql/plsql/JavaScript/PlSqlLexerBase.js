var Lexer = require('antlr4/index').Lexer;

function PlSqlBaseLexer(...args) {
    Lexer.call(this, ...args);
    return this;
}

PlSqlBaseLexer.prototype = Object.create(Lexer.prototype);
PlSqlBaseLexer.prototype.constructor = PlSqlBaseLexer;

PlSqlBaseLexer.prototype.IsNewlineAtPos = function (pos) {
    var la = this._input.LA(pos);
    return la == -1 || la == '\n';
}

exports.PlSqlBaseLexer = PlSqlBaseLexer;