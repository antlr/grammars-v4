
var Parser = require('antlr4/index').Parser;

function PlSqlBaseParser(...args) {
    Parser.call(this, ...args);
    this._isVersion10 = false;
    this._isVersion12 = true;
    return this;
}

PlSqlBaseParser.prototype = Object.create(Parser.prototype);
PlSqlBaseParser.prototype.constructor = PlSqlBaseParser;

PlSqlBaseParser.prototype.isVersion10 = function () {
    return this._isVersion10;
}
PlSqlBaseParser.prototype.isVersion12 = function () {
    return this._isVersion12;
}
PlSqlBaseParser.prototype.setVersion10 = function (value) {
    this._isVersion10 = value;
}
PlSqlBaseParser.prototype.setVersion12 = function (value) {
    this._isVersion12 = value;
}

exports.PlSqlBaseParser = PlSqlBaseParser;