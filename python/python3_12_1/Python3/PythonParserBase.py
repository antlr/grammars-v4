from antlr4 import InputStream, Parser
from typing import TextIO
import sys

class PythonParserBase(Parser):
    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

    def isEqualToCurrentTokenText(self, tokenText: str) -> bool:
        return self.getCurrentToken().text == tokenText

    def isnotEqualToCurrentTokenText(self, tokenText: str) -> bool:
        return not self.isEqualToCurrentTokenText(tokenText) # for compatibility with the '!' logical operator in other languages
