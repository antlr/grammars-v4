import {CommonToken, Lexer, Token, CharStream} from "antlr4ng";

export default abstract class Fortran90LexerBase extends Lexer {
    constructor(input: CharStream) {
        super(input);
    }

    IsColumnZero(): boolean {
        return this.column == 0;
    }

    VerifyNotOperator(): boolean
    {
        var c1 = this.inputStream.LA(1);
        if (c1 == 'a'.charCodeAt(0))
        {
            var c2 = this.inputStream.LA(2);
            if (c2 == 'n'.charCodeAt(0))
            {
                var c3 = this.inputStream.LA(3);
                if (c3 == 'd'.charCodeAt(0))
                {
                    var c4 = this.inputStream.LA(4);
                    if (c4 == '.'.charCodeAt(0))
                    {
                        return false;
                    }
                }
            }
        }
        else if (c1 == 'o'.charCodeAt(0))
        {
            var c2 = this.inputStream.LA(2);
            if (c2 == 'r'.charCodeAt(0))
            {
                var c3 = this.inputStream.LA(3);
                if (c3 == '.'.charCodeAt(0))
                {
                    return false;
                }
            }
        }
        return true;
    }
}
