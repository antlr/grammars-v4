from antlr4 import *

class PlSqlLexerBase(Lexer):

    def IsNewlineAtPos(self, pos):
        la = self._input.LA(pos)
        return la == -1 or la == 10 

    def IsQQuoteDelimiter(self):
        opening = self._input.LA(self._tokenStartCharIndex + 2 - self._input.index)
        closing = self._input.LA(-2)
        delimiters = {91: 93, 123: 125, 60: 62, 40: 41}
        return closing == delimiters.get(opening, opening)
