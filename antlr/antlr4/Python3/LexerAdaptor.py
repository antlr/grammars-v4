"""
 [The "BSD licence"]
 Copyright (c) 2005-2007 Terence Parr
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""
from antlr4 import *


class LexerAdaptor(Lexer):

    """
      Track whether we are inside of a rule and whether it is lexical parser. _currentRuleType==Token.INVALID_TYPE
      means that we are outside of a rule. At the first sign of a rule name reference and _currentRuleType==invalid, we
      can assume that we are starting a parser rule. Similarly, seeing a token reference when not already in rule means
      starting a token rule. The terminating ';' of a rule, flips this back to invalid type.

      This is not perfect logic but works. For example, "grammar T;" means that we start and stop a lexical rule for
      the "T;". Dangerous but works.

      The whole point of this state information is to distinguish between [..arg actions..] and [charsets]. Char sets
      can only occur in lexical rules and arg actions cannot occur.
    """

    _currentRuleType = Token.INVALID_TYPE

    def __init__(self, inp, output):
        Lexer.__init__(self, inp, output)

    def getCurrentRuleType(self):
        return self._currentRuleType

    def setCurrentRuleType(self, ruleType):
        self._currentRuleType = ruleType

    def handleBeginArgument(self):
        if self.inLexerRule():
            self.pushMode(self.LexerCharSet)
            self.more()
        else:
            self.pushMode(self.Argument)

    def handleEndArgument(self):
        self.popMode()
        if len(self._modeStack) > 0:
            self._type = self.ARGUMENT_CONTENT

    def handleEndAction(self):
        self.popMode()
        if len(self._modeStack) > 0:
            self._type = self.ACTION_CONTENT

    def emit(self):
        if self._type == self.ID:
            firstChar = self._input.getText(self._tokenStartCharIndex, self._tokenStartCharIndex)
            if firstChar[0].isupper():
                self._type = self.TOKEN_REF
            else:
                self._type = self.RULE_REF

            if self._currentRuleType == Token.INVALID_TYPE:  # if outside of rule def
                self._currentRuleType = self._type  # set to inside lexer or parser rule

        elif self._type == self.SEMI:  # exit rule def
            self._currentRuleType = Token.INVALID_TYPE
        return Lexer.emit(self)

    def inLexerRule(self):
        return self._currentRuleType == self.TOKEN_REF

    def inParserRule(self):  # not used, but added for clarity
        return self._currentRuleType == self.RULE_REF
