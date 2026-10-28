from antlr4 import *

class Fortran90LexerBase(Lexer):
    def IsColumnZero(self):
        return self.column == 0

    def VerifyNotOperator():
        c1 = this.InputStream.LA(1);
        if (c1 == 'a'):
            c2 = this.InputStream.LA(2);
            if (c2 == 'n'):
                c3 = this.InputStream.LA(3);
                if (c3 == 'd'):
                    c4 = this.InputStream.LA(4);
                    if (c4 == '.'):
                        return false;
        elif (c1 == 'o'):
            c2 = this.InputStream.LA(2);
            if (c2 == 'r'):
                c3 = this.InputStream.LA(3);
                if (c3 == '.'):
                    return false;
        return true;
