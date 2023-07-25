from typing import TextIO
from antlr4 import *
from antlr4.Token import CommonToken
from BisonLexer import BisonLexer
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
        ++percent_percent_count;
        if percent_percent_count == 1:
            return
        elif percent_percent_count == 2:
            this.pushMode(BisonLexer.EpilogueMode)
            return
        else:
            this.Type = BisonLexer.PercentPercent;
            return;
        self.opened += 1

