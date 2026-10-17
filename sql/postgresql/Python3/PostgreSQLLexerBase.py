# PostgreSQL grammar.
# The MIT License (MIT).
# Copyright (c) 2021-2023, Oleksii Kovalov (Oleksii.Kovalov@outlook.com).
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

from typing import TextIO
from antlr4 import *
from antlr4.Token import CommonToken
import sys
from typing import TextIO

class PostgreSQLLexerBase(Lexer):

    def IsColumnZero(self):
        return self.column == 0

    def VerifyNotOperator():
        c1 = this.InputStream.LA(1);
        if (c1 == 'a'):
            c2 = this.InputStream.LA(2);
            if (c2 == 'n'):
                c3 = this.InputStream.LA(3);
                if (c3 == 'd'):
                    c4 = this.InputStream.LA(4);
                    if (c4 == '.'):
                        return false;
        elif (c1 == 'o'):
            c2 = this.InputStream.LA(2);
            if (c2 == 'r'):
                c3 = this.InputStream.LA(3);
                if (c3 == '.'):
                    return false;
        return true;


class PostgreSQLLexerBase(Lexer):
    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)
        self.tags = []

    def PushTag(self):
        self.tags.append(self.text)

    def IsTag(self):
        return self.text == self.tags[-1] if self.tags else False

    def PopTag(self):
        if self.tags:
            self.tags.pop()

    def UnterminatedBlockCommentDebugAssert(self):
        assert self._input.LA(1) == -1  # EOF

    def CheckLaMinus(self):
        return self._input.LA(1) != ord('-')

    def CheckLaStar(self):
        return self._input.LA(1) != ord('*')

    def CharIsLetter(self):
        return chr(self._input.LA(-1)).isalpha()

    def HandleNumericFail(self):
        self._input.seek(self._input.index - 2)
        self.type = PostgreSQLLexer.INTEGRAL

    def HandleLessLessGreaterGreater(self):
        if self.text == "<<":
            self.type = PostgreSQLLexer.LESS_LESS
        elif self.text == ">>":
            self.type = PostgreSQLLexer.GREATER_GREATER

    def CheckIfUtf32Letter(self):
        try:
            char = chr(int.from_bytes((chr(self._input.LA(-2)) + chr(self._input.LA(-1))).encode("utf-32"), 'little'))
            return char.isalpha()
        except ValueError:
            return False

    def IsSemiColon(self):
        return chr(self._input.LA(1)) == ';'
