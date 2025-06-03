from typing import TextIO
from antlr4 import *
from antlr4.Token import CommonToken
import sys
from typing import TextIO

class LuaLexerBase(Lexer):

    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)
        self.start_line = 0
        self.start_col = 0

    def HandleComment(self):
        self.start_line = self.line
        self.start_col = self.column - 2
        cs = self._input

        if cs.LA(1) == 91:  # '['
            sep = self.skip_sep(cs)
            if sep >= 2:
                self.read_long_string(cs, sep)
                return

        while cs.LA(1) != 10 and cs.LA(1) != -1:  # '\n'
            self._interp.consume(cs)

    def read_long_string(self, cs:InputStream, sep:int):
        done = False
        self._interp.consume(cs)

        while not done:
            c = cs.LA(1)
            if c == -1:
                done = True
            elif c == 93:  # ']'
                if self.skip_sep(cs) == sep:
                    self._interp.consume(cs)
                    done = True
            else:
                if cs.LA(1) == -1:
                    done = True
                else:
                    self._interp.consume(cs)

    def skip_sep(self, cs:InputStream):
        count = 0
        s = cs.LA(1)
        self._interp.consume(cs)

        while cs.LA(1) == 61:  # '='
            self._interp.consume(cs)
            count += 1

        if cs.LA(1) == s:
            count += 2
        elif count == 0:
            count = 1
        else:
            count = 0

        return count

    def IsLine1Col0(self):
        cs = self._input
        return cs.index == 1


