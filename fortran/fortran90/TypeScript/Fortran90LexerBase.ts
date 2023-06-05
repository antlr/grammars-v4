import {Lexer, Token, CharStream} from "antlr4";

export abstract class Fortran90LexerBase extends Lexer {

    constructor(input: CharStream) {
        super(input);
    }

    IsColumnZero(): boolean {
	return this.column == 0;
    }
}
