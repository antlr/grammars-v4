import antlr4 from 'antlr4';
import Dart2Lexer from './Dart2Lexer.js';

export default class Dart2LexerBase extends antlr4.Lexer {
    constructor(input) {
        super(input);
    }
    CheckNotOpenBrace() {
        return this._input.LA(1) !== "{".codePointAt(0);
    }
}

