from antlr4 import *

class PlSqlBaseLexer(Lexer):

    def IsNewlineAtPos(self, pos):
        la = self._input.LA(pos)
        return la == -1 or la == 10 
