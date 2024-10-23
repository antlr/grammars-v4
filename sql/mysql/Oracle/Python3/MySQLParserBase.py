from antlr4 import *
import re
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO
from SqlMode import SqlMode

class MySQLParserBase(Parser):
    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.serverVersion = 0
        self.sqlModes = set()
        self.supportMle = True

    def isSqlModeActive(self, mode):
        """
        Determines if the given SQL mode is currently active in the lexer.

        :param mode: The mode to check.
        :return: True if the mode is one of the currently active modes.
        """
        return mode in self.sqlModes

    def isPureIdentifier(self):
        """
        Determines if a pure identifier is being parsed based on the active SQL mode.
        :return: True if the AnsiQuotes mode is active.
        """
        return self.isSqlModeActive(SqlMode.AnsiQuotes)

    def isTextStringLiteral(self):
        """
        Determines if the current parsing process treats the text as a string literal.
        :return: True if AnsiQuotes mode is not active.
        """
        return not self.isSqlModeActive(SqlMode.AnsiQuotes)

    def isStoredRoutineBody(self):
        return self.serverVersion >= 80032 and self.supportMle;

    def isSelectStatementWithInto(self):
        return self.serverVersion >= 80024 and self.serverVersion < 80031;
        