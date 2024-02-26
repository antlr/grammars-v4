from antlr4 import *

class Fortran77LexerBase(Lexer):
    def IsColumnZero(self):
        return self.column == 0
