import antlr4 from 'antlr4';

const COLON_CODE = ':'.charCodeAt(0);

export default class LexerBase extends antlr4.Lexer {
    Check1() {
        return this._input.LA(1) !== COLON_CODE;
    }
}
