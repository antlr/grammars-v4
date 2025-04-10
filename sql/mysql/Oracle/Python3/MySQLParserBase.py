from antlr4 import *
import re
import sys
if sys.version_info[1] > 5:
    from typing import TextIO
else:
    from typing.io import TextIO
from SqlMode import SqlMode
from SqlModes import SqlModes

class MySQLParserBase(Parser):
    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.serverVersion = 0
        self.sqlModes = SqlModes.sqlModeFromString("ANSI_QUOTES")
        self.supportMle = True
        self.serverVersion = 80200;

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

    def isServerVersionGe80004(self):
        return self.serverVersion >= 80004
        
    def isServerVersionGe80011(self):
        return self.serverVersion >= 80011
        
    def isServerVersionGe80013(self):
        return self.serverVersion >= 80013
        
    def isServerVersionGe80014(self):
        return self.serverVersion >= 80014
        
    def isServerVersionGe80016(self):
        return self.serverVersion >= 80016
        
    def isServerVersionGe80017(self):
        return self.serverVersion >= 80017
        
    def isServerVersionGe80018(self):
        return self.serverVersion >= 80018
        
    def isServerVersionGe80019(self):
        return self.serverVersion >= 80019
        
    def isServerVersionGe80024(self):
        return self.serverVersion >= 80024
        
    def isServerVersionGe80025(self):
        return self.serverVersion >= 80025
        
    def isServerVersionGe80027(self):
        return self.serverVersion >= 80027
        
    def isServerVersionGe80031(self):
        return self.serverVersion >= 80031
        
    def isServerVersionGe80032(self):
        return self.serverVersion >= 80032
        
    def isServerVersionGe80100(self):
        return self.serverVersion >= 80100
        
    def isServerVersionGe80200(self):
        return self.serverVersion >= 80200
        
    def isServerVersionLt80011(self):
        return self.serverVersion < 80011
        
    def isServerVersionLt80012(self):
        return self.serverVersion < 80012
        
    def isServerVersionLt80014(self):
        return self.serverVersion < 80014
        
    def isServerVersionLt80016(self):
        return self.serverVersion < 80016
        
    def isServerVersionLt80017(self):
        return self.serverVersion < 80017
        
    def isServerVersionLt80024(self):
        return self.serverVersion < 80024
        
    def isServerVersionLt80025(self):
        return self.serverVersion < 80025
        
    def isServerVersionLt80031(self):
        return self.serverVersion < 80031
