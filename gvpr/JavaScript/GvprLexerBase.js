import antlr4 from 'antlr4';
import gvprLexer from './gvprLexer.js';

export default class GvprLexerBase extends antlr4.Lexer {
    constructor(input) {
        super(input);
    }

	IsColumnZero() {
		return this.column == 1;
	}
}

