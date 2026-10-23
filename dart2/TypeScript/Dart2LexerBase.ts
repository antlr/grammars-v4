import { Lexer } from "antlr4";

export default abstract class Dart2LexerBase extends Lexer {
    CheckNotOpenBrace(): boolean {
        return this._input.LA(1) !== "{".codePointAt(0);
    }
}
