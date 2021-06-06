from antlr4 import *

class PlSqlLexerBase(Lexer):

    def IsNewlineAtPos(self, pos):
        la = self._input.LA(pos)
        return la == -1 or la == 10 
