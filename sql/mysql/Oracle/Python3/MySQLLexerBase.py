from antlr4 import *
import re
from collections import deque
import sys
if sys.version_info[1] > 5:
    from typing import TextIO
else:
    from typing.io import TextIO
from SqlMode import SqlMode
from SqlModes import SqlModes
from MySQLParser import MySQLParser

class MySQLLexerBase(Lexer):
    longString = "2147483647"
    longLength = 10
    signedLongString = "-2147483648"
    longLongString = "9223372036854775807"
    longLongLength = 19
    signedLongLongString = "-9223372036854775808"
    signedLongLongLength = 19
    unsignedLongLongString = "18446744073709551615"
    unsignedLongLongLength = 20

    def __init__(self, input=None, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.serverVersion = 0
        self.sqlModes = SqlModes.sqlModeFromString("ANSI_QUOTES")
        self.supportMle = True
        self.charSets = set()
        self.inVersionComment = False
        self.pendingTokens = deque()
        self.justEmitedDot = False
        self.serverVersion = 80200;

    def isSqlModeActive(self, mode):
        return mode in self.sqlModes

    def reset(self):
        self.inVersionComment = False
        super().reset()

    def nextToken(self):
        if self.pendingTokens:
            return self.pendingTokens.popleft()

        next_token = super().nextToken()
        if self.pendingTokens:
            self.pendingTokens.append(next_token)
            return self.pendingTokens.popleft()

        return next_token

    def checkMySQLVersion(self, text):
        if text is None:
            return False
        if len(text) < 8:
            return False
        version = int(text[3:])
        if version <= self.serverVersion:
            self.inVersionComment = True
            return True
        return False

    def determineFunction(self, proposed):
        input_char = chr(self._input.LA(1))
        if self.isSqlModeActive(SqlMode.IgnoreSpace):
            while input_char in " \t\r\n":
                self._input.consume()
                self._channel = Lexer.HIDDEN
                self._type = self.WHITESPACE
                input_char = chr(self._input.LA(1))

        return proposed if input_char == '(' else self.IDENTIFIER

    def determineNumericType(self, text):
        if text is None:
            return MySQLParser.INT_NUMBER

        length = len(text) - 1
        if length < MySQLLexerBase.longLength:
            return MySQLParser.INT_NUMBER

        negative = text[0] == '-'
        index = 1 if text[0] in '+-' else 0
        length -= index

        while text[index] == '0' and length > 0:
            index += 1
            length -= 1

        if length < MySQLLexerBase.longLength:
            return MySQLParser.INT_NUMBER

        smaller, bigger, cmp = None, None, None
        if negative:
            if length == MySQLLexerBase.longLength:
                cmp = MySQLLexerBase.signedLongString[1:]
                smaller = self.INT_NUMBER
                bigger = self.LONG_NUMBER
            elif length < MySQLLexerBase.signedLongLongLength:
                return self.LONG_NUMBER
            elif length > MySQLLexerBase.signedLongLongLength:
                return self.DECIMAL_NUMBER
            else:
                cmp = MySQLLexerBase.signedLongLongString[1:]
                smaller = self.LONG_NUMBER
                bigger = self.DECIMAL_NUMBER
        else:
            if length == MySQLLexerBase.longLength:
                cmp = MySQLLexerBase.longString
                smaller = self.INT_NUMBER
                bigger = self.LONG_NUMBER
            elif length < MySQLLexerBase.longLongLength:
                return self.LONG_NUMBER
            elif length > MySQLLexerBase.longLongLength:
                return self.DECIMAL_NUMBER if length > MySQLLexerBase.unsignedLongLongLength else self.ULONGLONG_NUMBER
            else:
                cmp = MySQLLexerBase.longLongString
                smaller = self.LONG_NUMBER
                bigger = self.ULONGLONG_NUMBER

        for idx, char in enumerate(cmp):
            if text[index + idx] != char:
                break

        return smaller if text[index + idx - 1] <= cmp[idx - 1] else bigger

    def checkCharset(self, text):
        return self.UNDERSCORE_CHARSET if text in self.charSets else self.IDENTIFIER

    def emitDot(self):
        self.pendingTokens.append(self._factory.create(self._tokenFactorySourcePair, self.DOT_SYMBOL, self._text, self._channel, self._tokenStartCharIndex, self._tokenStartCharIndex, self._tokenStartLine, self._tokenStartColumn))
        self._tokenStartColumn += 1
        self.justEmitedDot = True

    def emit(self):
        t = super().emit()
        if self.justEmitedDot:
            text = t.text
            text = text[1:]
            t.text = text
            t.start = t.start + 1
            # print(f"hi {text}", file=sys.stderr)
            #print(f"hi {t.text}", file=sys.stderr)
            self.justEmitedDot = False
        return t

    def isMasterCompressionAlgorithm(self):
        return self.serverVersion >= 80018 and self.isServerVersionLt80024();

    def isServerVersionGe80011(self):
        return self.serverVersion >= 80011

    def isServerVersionGe80013(self):
        return self.serverVersion >= 80013

    def isServerVersionLt80014(self):
        return self.serverVersion < 80014

    def isServerVersionGe80014(self):
        return self.serverVersion >= 80014

    def isServerVersionGe80016(self):
        return self.serverVersion >= 80016

    def isServerVersionGe80017(self):
        return self.serverVersion >= 80017

    def isServerVersionGe80018(self):
        return self.serverVersion >= 80018

    def isServerVersionLt80021(self):
        return self.serverVersion < 80021

    def isServerVersionGe80021(self):
        return self.serverVersion >= 80021

    def isServerVersionLt80022(self):
        return self.serverVersion < 80022

    def isServerVersionGe80022(self):
        return self.serverVersion >= 80022

    def isServerVersionLt80023(self):
        return self.serverVersion < 80023

    def isServerVersionGe80023(self):
        return self.serverVersion >= 80023

    def isServerVersionLt80024(self):
        return self.serverVersion < 80024

    def isServerVersionGe80024(self):
        return self.serverVersion >= 80024

    def isServerVersionLt80031(self):
        return self.serverVersion < 80031

    # Function handling methods
    def doLogicalOr(self):
        self._type = self.CONCAT_PIPES_SYMBOL if self.isSqlModeActive(SqlMode.PipesAsConcat) else self.LOGICAL_OR_OPERATOR

    def doIntNumber(self):
        self._type = self.determineNumericType(self._text)

    def doAdddate(self):
        self._type = self.determineFunction(self.ADDDATE_SYMBOL)

    def doBitAnd(self):
        self._type = self.determineFunction(self.BIT_AND_SYMBOL)

    def doBitOr(self):
        self._type = self.determineFunction(self.BIT_OR_SYMBOL)

    def doBitXor(self):
        self._type = self.determineFunction(self.BIT_XOR_SYMBOL)

    def doCast(self):
        self._type = self.determineFunction(self.CAST_SYMBOL)

    def doCount(self):
        self._type = self.determineFunction(self.COUNT_SYMBOL)

    def doCurdate(self):
        self._type = self.determineFunction(self.CURDATE_SYMBOL)

    def doCurrentDate(self):
        self._type = self.determineFunction(self.CURDATE_SYMBOL)

    def doCurrentTime(self):
        self._type = self.determineFunction(self.CURTIME_SYMBOL)

    def doCurtime(self):
        self._type = self.determineFunction(self.CURTIME_SYMBOL)

    def doDateAdd(self):
        self._type = self.determineFunction(self.DATE_ADD_SYMBOL)

    def doDateSub(self):
        self._type = self.determineFunction(self.DATE_SUB_SYMBOL)

    def doExtract(self):
        self._type = self.determineFunction(self.EXTRACT_SYMBOL)

    def doGroupConcat(self):
        self._type = self.determineFunction(self.GROUP_CONCAT_SYMBOL)

    def doMax(self):
        self._type = self.determineFunction(self.MAX_SYMBOL)

    def doMid(self):
        self._type = self.determineFunction(self.SUBSTRING_SYMBOL)

    def doMin(self):
        self._type = self.determineFunction(self.MIN_SYMBOL)

    def doNot(self):
        self._type = self.NOT2_SYMBOL if self.isSqlModeActive(SqlMode.HighNotPrecedence) else self.NOT_SYMBOL

    def doNow(self):
        self._type = self.determineFunction(self.NOW_SYMBOL)

    def doPosition(self):
        self._type = self.determineFunction(self.POSITION_SYMBOL)

    def doSessionUser(self):
        self._type = self.determineFunction(self.USER_SYMBOL)

    def doStddevSamp(self):
        self._type = self.determineFunction(self.STDDEV_SAMP_SYMBOL)

    def doStddev(self):
        self._type = self.determineFunction(self.STD_SYMBOL)

    def doStddevPop(self):
        self._type = self.determineFunction(self.STD_SYMBOL)

    def doStd(self):
        self._type = self.determineFunction(self.STD_SYMBOL)

    def doSubdate(self):
        self._type = self.determineFunction(self.SUBDATE_SYMBOL)

    def doSubstr(self):
        self._type = self.determineFunction(self.SUBSTRING_SYMBOL)

    def doSubstring(self):
        self._type = self.determineFunction(self.SUBSTRING_SYMBOL)

    def doSum(self):
        self._type = self.determineFunction(self.SUM_SYMBOL)

    def doSysdate(self):
        self._type = self.determineFunction(self.SYSDATE_SYMBOL)

    def doSystemUser(self):
        self._type = self.determineFunction(self.USER_SYMBOL)

    def doTrim(self):
        self._type = self.determineFunction(self.TRIM_SYMBOL)

    def doVariance(self):
        self._type = self.determineFunction(self.VARIANCE_SYMBOL)

    def doVarPop(self):
        self._type = self.determineFunction(self.VARIANCE_SYMBOL)

    def doVarSamp(self):
        self._type = self.determineFunction(self.VAR_SAMP_SYMBOL)

    def doUnderscoreCharset(self):
        self._type = self.checkCharset(self._text)

    def doDollarQuotedStringText(self):
        return self.serverVersion >= 80034 and self.supportMle;

    def isVersionComment(self):
        return self.checkMySQLVersion(self._text)

    def isBackTickQuotedId(self):
        return not self.isSqlModeActive(SqlMode.NoBackslashEscapes)

    def isDoubleQuotedText(self):
        return not self.isSqlModeActive(SqlMode.NoBackslashEscapes)

    def isSingleQuotedText(self):
        return not self.isSqlModeActive(SqlMode.NoBackslashEscapes)

    def startInVersionComment(self):
        self.inVersionComment = True

    def endInVersionComment(self):
        self.inVersionComment = False

    def isInVersionComment(self):
        return self.inVersionComment
