import {CommonToken, Lexer, Token, CharStream} from "antlr4ng";

export default abstract class LangiumLexerBase extends Lexer {
    constructor(input: CharStream) {
        super(input);
    }

    NoSlash(): boolean {
        return this.inputStream.LA(1) !== '/'.charCodeAt(0);
    }
}
