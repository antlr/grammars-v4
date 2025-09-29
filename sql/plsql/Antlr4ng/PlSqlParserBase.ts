import { Parser, TokenStream, CommonTokenStream, Recognizer } from 'antlr4ng';

export abstract class PlSqlParserBase extends Parser {

  _isVersion10: boolean;
  _isVersion11: boolean;
  _isVersion12: boolean;

  constructor(input: TokenStream) {
    super(input);
    this._isVersion10 = true;
    this._isVersion11 = true;
    this._isVersion12 = true;
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

}