import antlr4 from 'antlr4';
import JavaScriptLexer from './PlSqlParser.js';

export default class PlSqlParserBase extends antlr4.Parser {
  _isVersion10;
  _isVersion12;

  constructor(input) {
    super(input);
    this._isVersion10 = false;
    this._isVersion12 = true;
  }

  isVersion10() {
    return this._isVersion10;
  }

  isVersion12() {
    return this._isVersion12;
  }

  setVersion10(value) {
    this._isVersion10 = value;
  }

  setVersion12(value) {
    this._isVersion12 = value;
  }
}
