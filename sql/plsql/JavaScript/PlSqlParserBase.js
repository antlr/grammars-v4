import antlr4 from 'antlr4';
import JavaScriptLexer from './PlSqlParser.js';

export default class PlSqlParserBase extends antlr4.Parser {
  _isVersion10;
  _isVersion11;
  _isVersion12;

  constructor(input) {
    super(input);
    this._isVersion10 = true;
    this._isVersion11 = true;
    this._isVersion12 = true;
  }

  isVersion10() {
    return this._isVersion10;
  }

  isVersion11() {
    return this._isVersion11;
  }

  isVersion12() {
    return this._isVersion12;
  }

  setVersion10(value) {
    this._isVersion10 = value;
  }

  setVersion11(value) {
    this._isVersion11 = value;
  }

  setVersion12(value) {
    this._isVersion12 = value;
  }
}
