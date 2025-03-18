# --------------------------------------------------------------------------------
# [The "BSD licence"]
# Copyright (c) 2005-2007 Terence Parr
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions
# are met:
# 1. Redistributions of source code must retain the above copyright
#    notice, this list of conditions and the following disclaimer.
# 2. Redistributions in binary form must reproduce the above copyright
#    notice, this list of conditions and the following disclaimer in the
#    documentation and/or other materials provided with the distribution.
# 3. The name of the author may not be used to endorse or promote products
#    derived from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE AUTHOR "AS IS" AND ANY EXPRESS OR
# IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
# OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
# IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
# INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
# NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
# THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# --------------------------------------------------------------------------------

from antlr4 import *
import sys
from typing import TextIO
# NB!!!!!!!!!:
# Python3 is a terrible language. You cannot import ANTLRv4Lexer
# because Python3 cannot handle circular imports. So, we hardwire a
# bunch of constants directly in this damn code. If someone can
# figure out how to pound Python into submision and just work
# please fix.
# Cannot import lexer so use parser instead.
from ANTLRv4Parser import ANTLRv4Parser

class LexerAdaptor(Lexer):
    # Constants (mirroring the Java static final ints)
    PREQUEL_CONSTRUCT = -10
    OPTIONS_CONSTRUCT = -11

    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)
        # Track whether we are in a parser rule, lexer rule, or outside any rule.
        # Token.INVALID_TYPE indicates we are outside any rule context.
        self._currentRuleType = Token.INVALID_TYPE

    def getCurrentRuleType(self):
        return self._currentRuleType

    def setCurrentRuleType(self, ruleType):
        self._currentRuleType = ruleType

    def handleBeginArgument(self):
        """
        Decide how to handle a '[' token. If inside a lexer rule, it may be a character set;
        otherwise, it's a normal argument list and we switch to Argument mode.
        """
        if self.inLexerRule():
            self.pushMode(3); # NB!!!!!!!!!: hardwire ANTLRv4Parser.LexerCharSet)
            self.more()
        else:
            self.pushMode(1); # NB!!!!!!!!!: hardwire ANTLRv4Lexer.Argument)

    def handleEndArgument(self):
        """
        Upon finding a matching ']', pop out of Argument mode. If we still have a mode stack
        (i.e., nested contexts), set the type to ARGUMENT_CONTENT.
        """
        self.popMode()
        if len(self._modeStack) > 0:
            self.type = ANTLRv4Parser.ARGUMENT_CONTENT

    def emit(self):
        """
        Custom emit logic to update _currentRuleType based on tokens like OPTIONS, TOKENS,
        CHANNELS, RBRACE, AT, SEMI, ID, etc.
        """
        if (
            (self._type == ANTLRv4Parser.OPTIONS
             or self._type == ANTLRv4Parser.TOKENS
             or self._type == ANTLRv4Parser.CHANNELS)
            and self.getCurrentRuleType() == Token.INVALID_TYPE
        ):
            # Enter prequel construct block, which ends when we see a '}'
            self.setCurrentRuleType(self.PREQUEL_CONSTRUCT)

        elif (
            self._type == ANTLRv4Parser.OPTIONS
            and self.getCurrentRuleType() == ANTLRv4Parser.TOKEN_REF
        ):
            # We encountered OPTIONS inside a lexer rule, treat it specially
            self.setCurrentRuleType(self.OPTIONS_CONSTRUCT)

        elif (
            self._type == ANTLRv4Parser.RBRACE
            and self.getCurrentRuleType() == self.PREQUEL_CONSTRUCT
        ):
            # Exiting a prequel construct block
            self.setCurrentRuleType(Token.INVALID_TYPE)

        elif (
            self._type == ANTLRv4Parser.RBRACE
            and self.getCurrentRuleType() == self.OPTIONS_CONSTRUCT
        ):
            # Exiting an options block back into a lexer rule
            self.setCurrentRuleType(ANTLRv4Parser.TOKEN_REF)

        elif (
            self._type == ANTLRv4Parser.AT
            and self.getCurrentRuleType() == Token.INVALID_TYPE
        ):
            # Entering an action block
            self.setCurrentRuleType(ANTLRv4Parser.AT)

        elif (
            self._type == ANTLRv4Parser.SEMI
            and self.getCurrentRuleType() == self.OPTIONS_CONSTRUCT
        ):
            # A semicolon inside an options {...} block, do nothing special
            pass

        elif (
            self._type == ANTLRv4Parser.ACTION
            and self.getCurrentRuleType() == ANTLRv4Parser.AT
        ):
            # Exiting an action block
            self.setCurrentRuleType(Token.INVALID_TYPE)

        elif self._type == ANTLRv4Parser.ID:
            # Distinguish between TOKEN_REF (uppercase ID) vs. RULE_REF (lowercase ID)
            # If the input is only a single char, getText(...) should still return that char
            # You may need to import Interval or adapt how text is retrieved for your runtime
            from antlr4 import InputStream

            firstChar = self._input.getText(self._tokenStartCharIndex, self._tokenStartCharIndex)
            if firstChar and firstChar[0].isupper():
                self._type = ANTLRv4Parser.TOKEN_REF
            else:
                self._type = ANTLRv4Parser.RULE_REF

            # If we were outside a rule, now we're inside a rule of the indicated type
            if self.getCurrentRuleType() == Token.INVALID_TYPE:
                self.setCurrentRuleType(self._type)

        elif self._type == ANTLRv4Parser.SEMI:
            # The ';' token indicates the end of a rule definition
            self.setCurrentRuleType(Token.INVALID_TYPE)

        return super().emit()

    def inLexerRule(self):
        """
        True if we are currently inside a lexer rule (TOKEN_REF).
        """
        return self.getCurrentRuleType() == ANTLRv4Parser.TOKEN_REF

    def inParserRule(self):
        """
        True if we are currently inside a parser rule (RULE_REF).
        """
        return self.getCurrentRuleType() == ANTLRv4Parser.RULE_REF

    def reset(self):
        """
        Override reset to also reset the current rule type to INVALID_TYPE.
        """
        self.setCurrentRuleType(Token.INVALID_TYPE)
        super().reset()
