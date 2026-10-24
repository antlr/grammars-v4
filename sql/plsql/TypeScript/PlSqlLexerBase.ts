import { CommonToken, Lexer, CharStream, Token } from "antlr4";
import PlSqlParser from './PlSqlParser';

export default abstract class PlSqlLexerBase extends Lexer {
  IsNewlineAtPos(pos: number): boolean {
    const la = this._input.LA(pos);
    return la == -1 || String.fromCharCode(la) == '\n';
  }

  IsQQuoteDelimiter(): boolean {
    const openingOffset = this._tokenStartCharIndex + 2 - this._input.index;
    let opening = this._input.LA(openingOffset);
    let closing = this._input.LA(-2);
    if (opening >= 0xD800 && opening <= 0xDBFF) {
      opening = (opening - 0xD800) * 0x400 + this._input.LA(openingOffset + 1) - 0xDC00 + 0x10000;
    }
    if (closing >= 0xDC00 && closing <= 0xDFFF) {
      closing = (this._input.LA(-3) - 0xD800) * 0x400 + closing - 0xDC00 + 0x10000;
    }
    switch (opening) {
      case 91: return closing == 93;
      case 123: return closing == 125;
      case 60: return closing == 62;
      case 40: return closing == 41;
      default: return closing == opening;
    }
  }

}
