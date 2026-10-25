import antlr4 from 'antlr4';
import Fortran77Lexer from './Fortran77Lexer.js';

export default class Fortran77LexerBase extends antlr4.Lexer {
	IsColumnZero(pos) {
		return this.column == 0;
	}
}
