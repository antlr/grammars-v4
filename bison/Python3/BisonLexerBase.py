from typing import TextIO
from antlr4 import *
from antlr4.Token import CommonToken
import sys
from typing import TextIO

class BisonLexerBase(Lexer):

    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)
        self.percent_percent_count = 0
    
    def reset(self):
        self.percent_percent_count = 0
        super().reset()
    
    def NextMode(self):
        if "." in __name__:
            from .BisonLexer import BisonLexer
        else:
            from BisonLexer import BisonLexer
        self.percent_percent_count = self.percent_percent_count + 1;
        if self.percent_percent_count == 1:
            return
        elif self.percent_percent_count == 2:
            self.pushMode(BisonLexer.EpilogueMode)
            return
        else:
            self.type = BisonLexer.PercentPercent;
            return;
