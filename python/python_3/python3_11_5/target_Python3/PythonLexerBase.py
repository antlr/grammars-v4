# The MIT License (MIT)
# Copyright (c) 2021 Robert Einhorn
# 
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.

# Project      : Python Indent/Dedent handler for ANTLR4 grammars
# 
# Developed by : Robert Einhorn

from collections import deque
from typing import TextIO
from antlr4 import InputStream, Lexer, Token
from antlr4.Token import CommonToken
import sys
import re

class PythonLexerBase(Lexer):
    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

        # A stack that keeps track of the indentation lengths
        self._indent_lengths: Deque[int] = deque()
        # A list where tokens are waiting to be loaded into the token stream
        self._pending_tokens: list[CommonToken] = []
        # last pending token types
        self._previous_pending_token_type: int = 0
        self._last_pending_token_type_for_default_channel: int = 0

        # The amount of opened parentheses, square brackets or curly braces
        self._opened: int = 0

        # Was there a space char in the indentations?
        self._was_space_indentation: bool = False
        # The last number of the line of the indentation that used tab char
        self._last_line_of_tabbed_indentation: int = 0

        self._cur_token: CommonToken = None # current (under processing) token
        self._ffg_token: CommonToken = None # following (look ahead) token

    def nextToken(self) -> CommonToken: # reading the input stream until a return EOF
        self.check_next_token()
        return self._pending_tokens.pop(0) # add the queued token to the token stream

    def check_next_token(self):
        if self._previous_pending_token_type != Token.EOF:
            self.set_current_and_following_tokens()
            self.handle_start_of_input()
            match self._cur_token.type:
                case self.LPAR | self.LSQB | self.LBRACE: # OPEN_PAREN | OPEN_BRACK | OPEN_BRACE
                    self._opened += 1
                    self.add_pending_token(self._cur_token)
                case self.RPAR | self.RSQB | self.RBRACE: # CLOSE_PAREN | CLOSE_BRACK | CLOSE_BRACE
                    self._opened -= 1
                    self.add_pending_token(self._cur_token)
                case self.NEWLINE:
                    self.handle_NEWLINE_token()
                case self.STRING:
                    self.handle_STRING_token();
                case Token.EOF:
                    self.handle_EOF_token()
                case other:
                    self.add_pending_token(self._cur_token)

    def set_current_and_following_tokens(self):
        self._cur_token = super().nextToken() if self._ffg_token is None else \
                          self._ffg_token

        self._ffg_token = self._cur_token if self._cur_token.type == Token.EOF else \
                          super().nextToken()

    # initialize the _indent_lengths stack
    # hide the leading NEWLINE token(s)
    # if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    # insert a leading INDENT token if necessary
    def handle_start_of_input(self):
        if len(self._indent_lengths) == 0: # We're at the first token
            # initialize the stack with a default 0 indentation length
            self._indent_lengths.append(0) # this will never be popped off
            while self._cur_token.type != Token.EOF:
                if self._cur_token.channel == Token.DEFAULT_CHANNEL:
                    if self._cur_token.type == self.NEWLINE:
                        # all the NEWLINE tokens must be ignored before the first statement
                        self.hide_and_add_pending_token(self._cur_token)
                    else: # We're at the first statement
                        self.insert_leading_indent_token()
                        return # continue the processing of the current token with check_next_token()
                else:
                    self.add_pending_token(self._cur_token) # it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
                self.set_current_and_following_tokens()
            # continue the processing of the EOF token with check_next_token()

    def insert_leading_indent_token(self):
        if self._previous_pending_token_type == self.WS: # there is an "indentation" before the first statement
            # insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
            self.create_and_add_pending_token(self.INDENT, self._cur_token)

    def handle_NEWLINE_token(self):
        if self._opened > 0: # *** https://docs.python.org/3/reference/lexical_analysis.html#implicit-line-joining
            self.hide_and_add_pending_token(self._cur_token) # We're in an implicit line joining, ignore the current NEWLINE token
        else:
            nl_token: CommonToken = self._cur_token # save the current NEWLINE token
            isLookingAhead: bool = self._ffg_token.type == self.WS
            if isLookingAhead:
                self.set_current_and_following_tokens() # set the two next tokens

            match self._ffg_token.type:
                case self.NEWLINE | self.COMMENT | self.TYPE_COMMENT:
                    # We're before a blank line or a comment or a type comment
                    self.hide_and_add_pending_token(nl_token)     # ignore the NEWLINE token
                    if isLookingAhead:
                        self.add_pending_token(self._cur_token) # WS token
                case other:
                    self.add_pending_token(nl_token)
                    if isLookingAhead: # We're on a whitespace(s) followed by a statement
                        self.add_pending_token(self._cur_token) # WS token
                        indentationLength = 0 if self._ffg_token.type == Token.EOF else self.get_current_indentation_length()
                        self.insert_indent_or_dedent_token(indentationLength) # may insert INDENT token or DEDENT token(s)
                    else: # We're before a statement (there is no whitespace before the statement)
                        self.insert_indent_or_dedent_token(0) # may insert DEDENT token(s)

    def insert_indent_or_dedent_token(self, cur_indent_length: int):
        # *** https://docs.python.org/3/reference/lexical_analysis.html#indentation
        prev_indent_length: int = self._indent_lengths[0] # never has null value
        if cur_indent_length > prev_indent_length:
            self.create_and_add_pending_token(self.INDENT, self._ffg_token)
            self._indent_lengths.appendleft(cur_indent_length)
        else:
            while cur_indent_length < prev_indent_length: # more than 1 DEDENT token may be inserted to the token stream
                self._indent_lengths.popleft()
                prev_indent_length = self._indent_lengths[0]
                if cur_indent_length <= prev_indent_length:
                    self.create_and_add_pending_token(self.DEDENT, self._ffg_token)
                else:
                    pass
#                    IndentationErrorListener.lexer_error(" line " + str(self._ffg_token.line)
#                                                       + ": \t unindent does not match any outer indentation level")

    def handle_STRING_token(self): # remove the \<newline> escape sequences from the string literal
        # https://docs.python.org/3.11/reference/lexical_analysis.html#string-and-bytes-literals
        line_joinFreeStringLiteral: str = re.sub("\\\\\\r?\\n", "", self._cur_token.text)
        if len(self._cur_token.text) == len(line_joinFreeStringLiteral):
            self.add_pending_token(self._cur_token)
        else:
            originalSTRINGtoken: CommonToken = self._cur_token.clone() # backup the original token
            self._cur_token.text = line_joinFreeStringLiteral
            self.add_pending_token(self._cur_token)             # add the modified token with inline string literal
            self.hide_and_add_pending_token(originalSTRINGtoken) # add the original token with hidden channel
            # this hidden token allows to restore the original string literal with the \<newline> escape sequences

    def handle_EOF_token(self):
        if self._last_pending_token_type_for_default_channel > 0: # there was statement in the input (leading NEWLINE tokens are hidden)
            self.insert_trailing_tokens()
            self.check_space_and_tab_indentation()
        self.add_pending_token(self._cur_token) # EOF token

    def insert_trailing_tokens(self):
        match self._last_pending_token_type_for_default_channel:
            case self.NEWLINE | self.DEDENT:
                pass # no trailing NEWLINE token is needed
            case other:
                # insert an extra trailing NEWLINE token that serves as the end of the last statement
                self.create_and_add_pending_token(self.NEWLINE, self._ffg_token)
        self.insert_indent_or_dedent_token(0) # Now insert as much trailing DEDENT tokens as needed

    def hide_and_add_pending_token(self, token: CommonToken):
        token.channel = Token.HIDDEN_CHANNEL # channel=1
        self.add_pending_token(token)

    def create_and_add_pending_token(self, type: int, base_token: CommonToken):
        token: CommonToken = base_token.clone()
        token.type  = type
        token.channel = Token.DEFAULT_CHANNEL
        token.stop = base_token.start - 1
        token.text = "<" + self.symbolicNames[type] + ">"
        self.add_pending_token(token)

    def add_pending_token(self, token: CommonToken):
        # save the last pending token type because the _pending_tokens list can be empty by the nextToken()
        self._previous_pending_token_type = token.type
        if token.channel == Token.DEFAULT_CHANNEL:
            self._last_pending_token_type_for_default_channel = self._previous_pending_token_type
        self._pending_tokens.append(token) # the token will be added to the token stream

    # Calculates the indentation of the provided spaces, taking the
    # following rules into account:
    # 
    # "Tabs are replaced (from left to right) by one to eight spaces
    #  such that the total number of characters up to and including
    #  the replacement is a multiple of eight [...]"
    # 
    #  -- https://docs.python.org/3/reference/lexical_analysis.html#indentation
    def get_current_indentation_length(self) -> int:
        white_spaces: str = self._cur_token.text
        TAB_LENGTH: int = 8 # the standard number of spaces to replace a tab to spaces
        length: int = 0
        ch: str
        for ch in white_spaces:
            match ch:
                case ' ': # A normal space char
                    self._was_space_indentation = True
                    length += 1
                case '\t':
                    self._last_line_of_tabbed_indentation = self._cur_token.line
                    length += TAB_LENGTH - (length % TAB_LENGTH)
        return length

    def check_space_and_tab_indentation(self):
        if self._was_space_indentation and self._last_line_of_tabbed_indentation > 0:
            pass
#            IndentationErrorListener.lexer_error(" line " + str(self._last_line_of_tabbed_indentation)
#                                               + ":\t inconsistent use of tabs and spaces in indentation")
