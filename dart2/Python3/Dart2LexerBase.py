from typing import TextIO
from antlr4 import *
from antlr4.Token import CommonToken
import sys
from typing import TextIO

class Dart2LexerBase(Lexer):

    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

    def CheckNotOpenBrace(self):
        return self._input.LA(1) != 123;  # '{'


