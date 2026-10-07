from antlr4 import Lexer


class LexerBase(Lexer):
    def Check1(self):
        return self._input.LA(1) != ord(':')
