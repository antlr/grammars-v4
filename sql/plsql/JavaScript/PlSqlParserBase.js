import antlr4 from 'antlr4';
import JavaScriptLexer from './PlSqlParser.js';
import PlSqlLexer from './PlSqlLexer.js';

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

  IsNotNumericFunction() {
    const lt1 = this.getTokenStream().LT(1);
    const lt2 = this.getTokenStream().LT(2);
    if ((lt1.type === PlSqlLexer.SUM ||
      lt1.type === PlSqlLexer.COUNT ||
      lt1.type === PlSqlLexer.AVG ||
      lt1.type === PlSqlLexer.MIN ||
      lt1.type === PlSqlLexer.MAX ||
      lt1.type === PlSqlLexer.ROUND ||
      lt1.type === PlSqlLexer.LEAST ||
      lt1.type === PlSqlLexer.GREATEST) && lt2.type === PlSqlLexer.LEFT_PAREN)
      return false;
    return true;
  }
}
