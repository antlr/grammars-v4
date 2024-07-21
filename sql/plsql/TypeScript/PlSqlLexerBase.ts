import { CommonToken, Lexer, CharStream, Token } from "antlr4";
import PlSqlParser from './PlSqlParser';

export default abstract class PlSqlLexerBase extends Lexer {
  self : PlSqlLexerBase;
  
  IsNewlineAtPos(pos: number): boolean {
    const la = this._input.LA(pos);
    return la == -1 || String.fromCharCode(la) == '\n';
  }

}