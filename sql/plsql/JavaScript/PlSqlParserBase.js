import antlr4 from 'antlr4';
import JavaScriptLexer from './PlSqlParser.js';
import PlSqlLexer from './PlSqlLexer.js';

export default class PlSqlParserBase extends antlr4.Parser {
  _isVersion10;
  _isVersion11;
  _isVersion12;
  _lastUnitWasPlsql;

  constructor(input) {
    super(input);
    this._isVersion10 = true;
    this._isVersion11 = true;
    this._isVersion12 = true;
    this._lastUnitWasPlsql = false;
  }

  reset() {
    this._lastUnitWasPlsql = false;
    super.reset();
  }

  setLastUnitPlsql() { this._lastUnitWasPlsql = true; }
  setLastUnitSql()   { this._lastUnitWasPlsql = false; }
  isLastUnitSql()    { return !this._lastUnitWasPlsql; }
  isLastUnitPlsql()  { return this._lastUnitWasPlsql; }

  isSolidusSeparator() {
    const stream = this.getTokenStream();
    const solidus = stream.LT(1);
    if (solidus == null || solidus.type !== PlSqlLexer.SOLIDUS)
      return false;

    const solidusLine = solidus.line;

    const prev = stream.LT(-1);
    if (prev != null && prev.type !== antlr4.Token.EOF && prev.line === solidusLine)
      return false;

    const next = stream.LT(2);
    if (next != null && next.type !== antlr4.Token.EOF && next.line === solidusLine)
      return false;

    return true;
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

  isNotStartOfJoin() {
    const lt1 = this.getTokenStream().LT(1);
    if (lt1.type == PlSqlLexer.INNER ||
        lt1.type == PlSqlLexer.CROSS ||
        lt1.type == PlSqlLexer.NATURAL ||
        lt1.type == PlSqlLexer.PARTITION ||
        lt1.type == PlSqlLexer.FULL ||
        lt1.type == PlSqlLexer.LEFT ||
        lt1.type == PlSqlLexer.RIGHT ||
        lt1.type == PlSqlLexer.OUTER)
        return false;
    return true;
  }
}
