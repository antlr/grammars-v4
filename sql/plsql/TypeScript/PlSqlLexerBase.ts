import { Lexer } from "antlr4ts";

export abstract class PlSqlLexerBase extends Lexer {
  
  IsNewlineAtPos(pos: number): boolean {
    const la = this._input.LA(pos);
    return la == -1 || String.fromCharCode(la) == '\n';
  }

}