import antlr4 from 'antlr4';
import Fortran90Lexer from './Fortran90Lexer.js';

export default class Fortran90LexerBase extends antlr4.Lexer {
	IsColumnZero(pos) {
		return this.column == 0;
	}
}
