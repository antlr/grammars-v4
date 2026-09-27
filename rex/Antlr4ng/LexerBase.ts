import { Lexer } from "antlr4ng";

export default abstract class LexerBase extends Lexer {
    public Check1(): boolean {
        return this.inputStream.LA(1) !== 58;
    }
}
