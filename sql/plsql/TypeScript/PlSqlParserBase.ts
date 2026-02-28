import { Parser, TokenStream } from "antlr4";
import PlSqlLexer from './PlSqlLexer.js';

export default abstract class PlSqlParserBase extends Parser {

  _isVersion10: boolean;
  _isVersion11: boolean;
  _isVersion12: boolean;
  self: PlSqlParserBase;

  constructor(input: TokenStream) {
    super(input);
    this._isVersion10 = false;
    this._isVersion11 = true;
    this._isVersion12 = true;
    this.self = this;
  }

  isVersion10(): boolean {
    return this._isVersion10;
  }

  isVersion11(): boolean {
    return this._isVersion11;
  }

  isVersion12(): boolean {
    return this._isVersion12;
  }

  setVersion10(value: boolean): void {
    this._isVersion10 = value;
  }

  setVersion11(value: boolean): void {
    this._isVersion11 = value;
  }

  setVersion12(value: boolean): void {
    this._isVersion12 = value;
  }

  IsNotNumericFunction(): boolean {
    const lt1 = this.getTokenStream().LT(1);
    const lt2 = this.getTokenStream().LT(2);
    if ((lt1!.type === PlSqlLexer.SUM ||
      lt1!.type === PlSqlLexer.COUNT ||
      lt1!.type === PlSqlLexer.AVG ||
      lt1!.type === PlSqlLexer.MIN ||
      lt1!.type === PlSqlLexer.MAX ||
      lt1!.type === PlSqlLexer.ROUND ||
      lt1!.type === PlSqlLexer.LEAST ||
      lt1!.type === PlSqlLexer.GREATEST) && lt2!.type === PlSqlLexer.LEFT_PAREN)
      return false;
    return true;
  }

}