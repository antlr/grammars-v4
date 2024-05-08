from antlr4 import *

class Fortran90LexerBase(Lexer):
    def IsColumnZero(self):
        return self.column == 0

    def VerifyNotOperator():
        var c1 = this.InputStream.LA(1);
        if (c1 == 'a'):
            var c2 = this.InputStream.LA(2);
            if (c2 == 'n'):
                var c3 = this.InputStream.LA(3);
                if (c3 == 'd'):
                    var c4 = this.InputStream.LA(4);
                    if (c4 == '.'):
                        return false;
        else if (c1 == 'o'):
            var c2 = this.InputStream.LA(2);
            if (c2 == 'r'):
                var c3 = this.InputStream.LA(3);
                if (c3 == '.'):
                    return false;
        return true;
