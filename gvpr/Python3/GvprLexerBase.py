from typing import TextIO
from antlr4 import *
from antlr4.Token import CommonToken
import sys
from typing import TextIO

class GvprLexerBase(Lexer):

    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

    def IsColumnZero(self):
        return self.column == 1


