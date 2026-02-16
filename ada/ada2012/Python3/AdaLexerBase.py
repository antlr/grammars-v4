from antlr4 import *
import sys
from typing import TextIO


class AdaLexerBase(Lexer):

    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)
        self._lastTokenType = 0

    def nextToken(self):
        token = super().nextToken()
        if token.channel == Token.DEFAULT_CHANNEL:
            self._lastTokenType = token.type
        return token

    def IsCharLiteralAllowed(self):
        # In Ada, a tick after an identifier, closing paren, or 'all' keyword
        # is an attribute tick, not the start of a character literal.
        if "." in __name__:
            from .AdaLexer import AdaLexer
        else:
            from AdaLexer import AdaLexer
        return (self._lastTokenType != AdaLexer.IDENTIFIER_
            and self._lastTokenType != AdaLexer.RP
            and self._lastTokenType != AdaLexer.ALL)
