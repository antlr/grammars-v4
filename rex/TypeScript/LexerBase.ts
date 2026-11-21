import { Lexer } from "antlr4";

export default abstract class LexerBase extends Lexer {
    public Check1(): boolean {
        return this._input.LA(1) !== 58;
    }
}
