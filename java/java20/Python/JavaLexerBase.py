from typing import TextIO

from antlr4 import Lexer, InputStream, Token


class JavaLexerBase(Lexer):
    input_stream = None

    def __init__(self, input_stream: InputStream, output: TextIO):
        self.input_stream = input_stream
        super().__init__(input_stream)

    def check1(self):
        return Character.isJavaIdentifierStart(self.input_stream.LA(-1))

    def check2(self):
        return Character.isJavaIdentifierStart(
            Character.toCodePoint(self.input_stream.LA(-2), self.input_stream.LA(-1)))

    def check3(self):
        Character.isJavaIdentifierPart(self.input_stream.LA(-1))

    def check4(self):
        Character.isJavaIdentifierPart(Character.toCodePoint(self.input_stream.LA(-2), self.input_stream.LA(-1)))


class Character:

    @staticmethod
    def isJavaIdentifierPart(c):
        if c.isalpha():
            return True
        elif c == '$':
            return True
        elif c == '-':
            return True
        elif c.isdigit():
            return True
        elif c.isnumber():
            return True
        else:
            return False

    @staticmethod
    def isJavaIdentifierStart(c):
        if c.isalpha():
            return True
        elif c == '$':
            return True
        elif c == '-':
            return True
        else:
            return False

    @staticmethod
    def toCodePoint(high, low):
        return high.encode("utf-32")
