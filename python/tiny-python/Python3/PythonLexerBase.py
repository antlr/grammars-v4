##The MIT License (MIT)
##Copyright (c) 2021 Robert Einhorn
##
##Permission is hereby granted, free of charge, to any person obtaining a copy
##of this software and associated documentation files (the "Software"), to deal
##in the Software without restriction, including without limitation the rights
##to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
##copies of the Software, and to permit persons to whom the Software is
##furnished to do so, subject to the following conditions:
##The above copyright notice and this permission notice shall be included in
##all copies or substantial portions of the Software.
##THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
##IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
##FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
##AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
##LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
##OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
##THE SOFTWARE.

## Project      : Python Indent/Dedent handler for ANTLR4 grammars
##
## Developed by : Robert Einhorn

from collections import deque
from typing import TextIO
from antlr4 import InputStream, Lexer, Token
from antlr4.Token import CommonToken
from IndentationErrorListener import IndentationErrorListener
import sys
import re

class PythonLexerBase(Lexer):
    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

        # A stack that keeps track of the indentation lengths
        self._indentLengths = deque()
        # A list where tokens are waiting to be loaded into the token stream
        self._pendingTokens = []
        # last pending token types
        self._previousPendingTokenType = 0
        self._lastPendingTokenTypeForDefaultChannel = 0

        # The amount of opened parentheses, square brackets or curly braces
        self._opened = 0

        # Was there a space char in the indentations?
        self._wasSpaceIndentation = False
        # The last number of the line of the indentation that used tab char
        self._lastLineOfTabbedIndentation = 0

        self._curToken = None # current (under processing) token
        self._ffgToken = None # following (look ahead) token

    def nextToken(self) -> CommonToken: # reading the input stream until a return EOF
        if self._input.size == 0:
            return CommonToken(Token.EOF, "<EOF>")
        else:
            self.checkNextToken()
            return self._pendingTokens.pop(0) # add the queued token to the token stream

    def checkNextToken(self):
        if self._previousPendingTokenType != Token.EOF:
            self.setCurrentAndFollowingTokens()
            self.handleStartOfInput()
            match self._curToken.type:
                case self.LPAR | self.LSQB | self.LBRACE: # OPEN_PAREN | OPEN_BRACK | OPEN_BRACE
                    self._opened += 1
                    self.addPendingToken(self._curToken)
                case self.RPAR | self.RSQB | self.RBRACE: # CLOSE_PAREN | CLOSE_BRACK | CLOSE_BRACE
                    self._opened -= 1
                    self.addPendingToken(self._curToken)
                case self.NEWLINE:
                    self.handleNEWLINEtoken()
                case self.STRING:
                    self.handleSTRINGtoken();
                case Token.EOF:
                    self.handleEOFtoken()
                case other:
                    self.addPendingToken(self._curToken)

    def setCurrentAndFollowingTokens(self):
        self._curToken = super().nextToken() if self._ffgToken is None else self._ffgToken
        self._ffgToken = self._curToken if self._curToken.type == Token.EOF else super().nextToken()

    ## initialize the _indentLengths stack
    ## hide the leading NEWLINE token(s)
    ## if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    ## insert a leading INDENT token if necessary
    def handleStartOfInput(self):
        if len(self._indentLengths) == 0: # We're at the first token
            # initialize the stack with a default 0 indentation length
            self._indentLengths.append(0) # this will never be popped off
            while self._curToken.type != Token.EOF:
                if self._curToken.channel == Token.DEFAULT_CHANNEL:
                    if self._curToken.type == self.NEWLINE:
                        # all the NEWLINE tokens must be ignored before the first statement
                        self.hideAndAddPendingToken(self._curToken)
                    else: # We're at the first statement
                        self.insertLeadingIndentToken()
                        return # continue the processing of the current token with checkNextToken()
                else:
                    self.addPendingToken(self._curToken) # it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
                self.setCurrentAndFollowingTokens()
            # continue the processing of the EOF token with checkNextToken()

    def insertLeadingIndentToken(self):
        if self._previousPendingTokenType == self.WS: # there is an "indentation" before the first statement
            # insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
            self.createAndAddPendingToken(self.INDENT, self._curToken) # insert an INDENT token before the _curToken

    def handleNEWLINEtoken(self):
        if self._opened > 0: # *** https://docs.python.org/3/reference/lexical_analysis.html#implicit-line-joining
            self.hideAndAddPendingToken(self._curToken) # We're in an implicit line joining, ignore the current NEWLINE token
        else:
            nlToken = self._curToken # save the current NEWLINE token
            isLookingAhead = self._ffgToken.type == self.WS
            if isLookingAhead:
                self.setCurrentAndFollowingTokens() # set the two next tokens

            match self._ffgToken.type:
                case self.NEWLINE | self.COMMENT | self.TYPE_COMMENT:
                    # We're before a blank line or a comment or a type comment
                    self.hideAndAddPendingToken(nlToken)     # ignore the NEWLINE token
                    if isLookingAhead:
                        self.addPendingToken(self._curToken) # WS token
                case other:
                    self.addPendingToken(nlToken)
                    if isLookingAhead: # We're on a whitespace(s) followed by a statement
                        self.addPendingToken(self._curToken) # WS token
                        indentationLength = 0 if self._ffgToken.type == Token.EOF else self.getCurrentIndentationLength()
                        self.insertIndentOrDedentToken(indentationLength) # may insert INDENT token or DEDENT token(s)
                    else: # We're before a statement (there is no whitespace before the statement)
                        self.insertIndentOrDedentToken(0) # may insert DEDENT token(s)

    def insertIndentOrDedentToken(self, curIndentLength: int):
        # *** https://docs.python.org/3/reference/lexical_analysis.html#indentation
        prevIndentLength: int = self._indentLengths[0] # never has null value
        if curIndentLength > prevIndentLength:
            self.createAndAddPendingToken(self.INDENT, self._ffgToken) # insert an INDENT token before the _ffgToken
            self._indentLengths.appendleft(curIndentLength)
        else:
            while curIndentLength < prevIndentLength: # more than 1 DEDENT token may be inserted to the token stream
                self._indentLengths.popleft()
                prevIndentLength = self._indentLengths[0]
                self.createAndAddPendingToken(self.DEDENT, self._ffgToken) # insert a DEDENT token before the _ffgToken
                if curIndentLength > prevIndentLength:
                    pass
##                    IndentationErrorListener.lexerError(" line " + str(self._ffgToken.line)
##                                                      + ": \t unindent does not match any outer indentation level")

    def handleSTRINGtoken(self): # remove the \<newline> escape sequences from the string literal
        # https://docs.python.org/3.11/reference/lexical_analysis.html#string-and-bytes-literals
        line_joinFreeStringLiteral = re.sub("\\\\\\r?\\n", "", self._curToken.text)
        if len(self._curToken.text) == len(line_joinFreeStringLiteral):
            self.addPendingToken(self._curToken)
        else:
            originalSTRINGtoken = self._curToken.clone() # backup the original token
            self._curToken.text = line_joinFreeStringLiteral
            self.addPendingToken(self._curToken)             # add the modified token with inline string literal
            self.hideAndAddPendingToken(originalSTRINGtoken) # add the original token with hidden channel
            # this hidden token allows to restore the original string literal with the \<newline> escape sequences

    def handleEOFtoken(self):
        if self._lastPendingTokenTypeForDefaultChannel > 0: # there was statement in the input (leading NEWLINE tokens are hidden)
            self.insertTrailingTokens()
            self.checkSpaceAndTabIndentation()
        self.addPendingToken(self._curToken) # EOF token

    def insertTrailingTokens(self):
        match self._lastPendingTokenTypeForDefaultChannel:
            case self.NEWLINE | self.DEDENT:
                pass # no trailing NEWLINE token is needed
            case other:
                # insert an extra trailing NEWLINE token that serves as the end of the last statement
                self.createAndAddPendingToken(self.NEWLINE, self._ffgToken) # insert before the _ffgToken
        self.insertIndentOrDedentToken(0) # Now insert as much trailing DEDENT tokens as needed

    def hideAndAddPendingToken(self, token: CommonToken):
        token.channel = Token.HIDDEN_CHANNEL # channel=1
        self.addPendingToken(token)

    def createAndAddPendingToken(self, type: int, followingToken: CommonToken): # insert a token before the followingToken
        token = CommonToken(followingToken.source
                          , type
                          , Token.DEFAULT_CHANNEL
                          , followingToken.start
                          , followingToken.start - 1)

        token.text = "<" + self.symbolicNames[type] + ">"
        token.line = followingToken.line
        token.column = followingToken.column
        self.addPendingToken(token)

    def addPendingToken(self, token: CommonToken):
        # save the last pending token type because the _pendingTokens list can be empty by the nextToken()
        self._previousPendingTokenType = token.type
        if token.channel == Token.DEFAULT_CHANNEL:
            self._lastPendingTokenTypeForDefaultChannel = self._previousPendingTokenType
        self._pendingTokens.append(token) # the token will be added to the token stream

    ## Calculates the indentation of the provided spaces, taking the
    ## following rules into account:
    ##
    ## "Tabs are replaced (from left to right) by one to eight spaces
    ##  such that the total number of characters up to and including
    ##  the replacement is a multiple of eight [...]"
    ##
    ##  -- https://docs.python.org/3/reference/lexical_analysis.html#indentation
    def getCurrentIndentationLength(self) -> int:
        whiteSpaces = self._curToken.text
        TAB_LENGTH = 8 # the standard number of spaces to replace a tab to spaces
        length = 0
        for i in range(len(whiteSpaces)):
            if whiteSpaces[i] == ' ': # A normal space char
                self._wasSpaceIndentation = True
                length += 1
            elif whiteSpaces[i] == '\t':
                self._lastLineOfTabbedIndentation = self._curToken.line
                length += TAB_LENGTH - (length % TAB_LENGTH)
        return length

    def checkSpaceAndTabIndentation(self):
        if self._wasSpaceIndentation and self._lastLineOfTabbedIndentation > 0:
            pass
##            IndentationErrorListener.lexerError(" line " + str(self._lastLineOfTabbedIndentation)
##                                              + ":\t inconsistent use of tabs and spaces in indentation")
