import { CommonToken, Lexer, CharStream, Token } from "antlr4";
import gvprLexer from './gvprLexer';

export default abstract class GvprLexerBase extends Lexer {

    constructor(input: CharStream) {
        super(input);
    }

    IsColumnZero() : boolean {
        return this.column == 1;
    }
}

