import { Parser, TokenStream } from "antlr4";

export default abstract class PlSqlParserBase extends Parser {

  _isVersion10: boolean;
  _isVersion12: boolean;
  self: PlSqlParserBase;

  constructor(input: TokenStream) {
    super(input);
    this._isVersion10 = false;
    this._isVersion12 = true;
    this.self = this;
  }

  isVersion10(): boolean {
    return this._isVersion10;
  }

  isVersion12(): boolean {
    return this._isVersion12;
  }

  setVersion10(value: boolean): void {
    this._isVersion10 = value;
  }

  setVersion12(value: boolean): void {
    this._isVersion12 = value;
  }

}