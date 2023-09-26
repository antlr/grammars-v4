##
## Project      : a helper class to implement specific PEG grammar expressions in an ANTLR4 grammar
##
## Developed by : Robert Einhorn
##

## Related PEG grammar expressions:
## &e
## https://peps.python.org/pep-0617/#e-3
##
## !e
## https://peps.python.org/pep-0617/#e-4

from antlr4 import InputStream, Parser
from typing import TextIO
import sys

class PythonParserBase(Parser):
    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

    def isEqualCurrentTokenText(self, tokenText: str) -> bool:
        return self.getCurrentToken().text == tokenText

    def isnotEqualCurrentTokenText(self, tokenText: str) -> bool:
        return not self.isEqualCurrentTokenText(tokenText) # for compatibility with the '!' logical operator in other languages
