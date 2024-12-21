import { CommonToken, Lexer, CharStream, Token, CommonTokenStream } from "antlr4ng";

export abstract class PlSqlLexerBase extends Lexer {
  self : PlSqlLexerBase;
  
  IsNewlineAtPos(pos: number): boolean {
    const la = this.inputStream.LA(pos);
    return la == -1 || String.fromCharCode(la) == '\n';
  }

}