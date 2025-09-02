import { CharStream, Lexer, Token } from "antlr4ng";

export default abstract class ASN_3gppLexerBase extends Lexer {
    constructor(input: CharStream) {
        super(input);
    }

    IsColumnZero(): boolean {
	return this.column == 0;
    }
}
