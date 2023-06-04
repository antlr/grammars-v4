import { Lexer } from "antlr4ts";

export abstract class Fortran90LexerBase extends Lexer {
    IsColumnZero(): boolean {
        return this.column() == 0;
    }
  IsNewlineAtPos(pos: number): boolean {
    const la = this._input.LA(pos);
    return la == -1 || String.fromCharCode(la) == '\n';
  }
}