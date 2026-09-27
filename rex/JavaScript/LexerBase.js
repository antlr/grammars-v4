import antlr4 from 'antlr4';

export default class LexerBase extends antlr4.Lexer {
    Check1() {
        return this._input.LA(1) !== 58;
    }
}
