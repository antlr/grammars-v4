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
        self.serverVersion = 80200;
        self.sqlModeFromString("ANSI_QUOTES");

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

    def sqlModeFromString(self, modes):
        self.sqlModes.clear()
        parts = modes.upper().split(",")
        for mode in parts:
            if mode in {"ANSI", "DB2", "MAXDB", "MSSQL", "ORACLE", "POSTGRESQL"}:
                self.sqlModes.update({SqlMode.AnsiQuotes, SqlMode.PipesAsConcat, SqlMode.IgnoreSpace})
            elif mode == "ANSI_QUOTES":
                self.sqlModes.add(SqlMode.AnsiQuotes)
            elif mode == "PIPES_AS_CONCAT":
                self.sqlModes.add(SqlMode.PipesAsConcat)
            elif mode == "NO_BACKSLASH_ESCAPES":
                self.sqlModes.add(SqlMode.NoBackslashEscapes)
            elif mode == "IGNORE_SPACE":
                self.sqlModes.add(SqlMode.IgnoreSpace)
            elif mode in {"HIGH_NOT_PRECEDENCE", "MYSQL323", "MYSQL40"}:
                self.sqlModes.add(SqlMode.HighNotPrecedence)

