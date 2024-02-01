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
        self.indent_length_stack: Deque[int]

        # A list where tokens are waiting to be loaded into the token stream
        self.pending_tokens: list[CommonToken]

        # last pending token types
        self.previous_pending_token_type: int
        self.last_pending_token_type_from_default_channel: int

        # The amount of opened parentheses, square brackets or curly braces
        self.opened: int

        self.was_space_indentation: bool
        self.was_tab_indentation: bool
        self.was_indentation_mixed_with_spaces_and_tabs: bool
        self.INVALID_LENGTH: int

        self.cur_token: CommonToken # current (under processing) token
        self.ffg_token: CommonToken # following (look ahead) token

        self.ERR_TXT: str

        self.init()

    def init(self):
        self.indent_length_stack = deque()
        self.pending_tokens = []
        self.previous_pending_token_type = 0
        self.last_pending_token_type_from_default_channel = 0
        self.opened = 0
        self.was_space_indentation = False
        self.was_tab_indentation = False
        self.was_indentation_mixed_with_spaces_and_tabs = False
        self.INVALID_LENGTH = -1
        self.cur_token = None
        self.ffg_token = None
        self.ERR_TXT = " ERROR: "

    def nextToken(self) -> CommonToken: # reading the input stream until a return EOF
        self.check_next_token()
        return self.pending_tokens.pop(0) # add the queued token to the token stream

    def check_next_token(self):
        if self.previous_pending_token_type != Token.EOF:
            self.set_current_and_following_tokens()
            if len(self.indent_length_stack) == 0: # We're at the first token
                self.handle_start_of_input()
            match self.cur_token.type:
                case self.LPAR | self.LSQB | self.LBRACE:
                    self.opened += 1
                    self.add_pending_token(self.cur_token)
                case self.RPAR | self.RSQB | self.RBRACE:
                    self.opened -= 1
                    self.add_pending_token(self.cur_token)
                case self.NEWLINE:
                    self.handle_NEWLINE_token()
                case self.STRING:
                    self.handle_STRING_token()
                case self.ERROR_TOKEN:
                    self.report_lexer_error("token recognition error at: '" + self.cur_token.text + "'")
                    self.add_pending_token(self.cur_token)
                case Token.EOF:
                    self.handle_EOF_token()
                case other:
                    self.add_pending_token(self.cur_token)

    def set_current_and_following_tokens(self):
        self.cur_token = super().nextToken() if self.ffg_token is None else \
                          self.ffg_token

        self.ffg_token = self.cur_token if self.cur_token.type == Token.EOF else \
                          super().nextToken()

    # initialize the _indent_length_stack
    # hide the leading NEWLINE token(s)
    # if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    # insert a leading INDENT token if necessary
    def handle_start_of_input(self):
        # initialize the stack with a default 0 indentation length
        self.indent_length_stack.append(0) # this will never be popped off
        while self.cur_token.type != Token.EOF:
            if self.cur_token.channel == Token.DEFAULT_CHANNEL:
                if self.cur_token.type == self.NEWLINE:
                    # all the NEWLINE tokens must be ignored before the first statement
                    self.hide_and_add_pending_token(self.cur_token)
                else: # We're at the first statement
                    self.insert_leading_indent_token()
                    return # continue the processing of the current token with check_next_token()
            else:
                self.add_pending_token(self.cur_token) # it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            self.set_current_and_following_tokens()
        # continue the processing of the EOF token with check_next_token()

    def insert_leading_indent_token(self):
        if self.previous_pending_token_type == self.WS:
            prev_token: CommonToken = self.pending_tokens[-1]  # WS token
            if self.get_indentation_length(prev_token.text) != 0: # there is an "indentation" before the first statement
                err_msg: str = "first statement indented"
                self.report_lexer_error(err_msg)
                # insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                self.create_and_add_pending_token(self.INDENT, Token.DEFAULT_CHANNEL, self.ERR_TXT + err_msg, self.cur_token)

    def handle_NEWLINE_token(self):
        if self.opened > 0: # We're in an implicit line joining, ignore the current NEWLINE token
            self.hide_and_add_pending_token(self.cur_token)
        else:
            nl_token: CommonToken = self.cur_token # save the current NEWLINE token
            is_looking_ahead: bool = self.ffg_token.type == self.WS
            if is_looking_ahead:
                self.set_current_and_following_tokens() # set the next two tokens

            match self.ffg_token.type:
                case self.NEWLINE | self.COMMENT:
                    # We're before a blank line or a comment or a type comment
                    self.hide_and_add_pending_token(nl_token)     # ignore the NEWLINE token
                    if is_looking_ahead:
                        self.add_pending_token(self.cur_token) # WS token
                case other:
                    self.add_pending_token(nl_token)
                    if is_looking_ahead: # We're on a whitespace(s) followed by a statement
                        indentation_length: int = 0 if self.ffg_token.type == Token.EOF else \
                                                  self.get_indentation_length(self.cur_token.text)

                        if indentation_length != self.INVALID_LENGTH:
                            self.add_pending_token(self.cur_token) # WS token
                            self.insert_indent_or_dedent_token(indentation_length) # may insert INDENT token or DEDENT token(s)
                        else:
                            self.report_error("inconsistent use of tabs and spaces in indentation")
                    else: # We're at a newline followed by a statement (there is no whitespace before the statement)
                        self.insert_indent_or_dedent_token(0) # may insert DEDENT token(s)

    def insert_indent_or_dedent_token(self, indent_length: int):
        prev_indent_length: int = self.indent_length_stack[-1] # peek()
        if indent_length > prev_indent_length:
            self.create_and_add_pending_token(self.INDENT, Token.DEFAULT_CHANNEL, None, self.ffg_token)
            self.indent_length_stack.append(indent_length)
        else:
            while indent_length < prev_indent_length: # more than 1 DEDENT token may be inserted to the token stream
                self.indent_length_stack.pop()
                prev_indent_length = self.indent_length_stack[-1] # peek()
                if indent_length <= prev_indent_length:
                    self.create_and_add_pending_token(self.DEDENT, Token.DEFAULT_CHANNEL, None, self.ffg_token)
                else:
                    self.report_error("inconsistent dedent")

    def handle_STRING_token(self): # remove the \<newline> escape sequences from the string literal
        # https://docs.python.org/3.11/reference/lexical_analysis.html#string-and-bytes-literals
        line_joinFreeStringLiteral: str = re.sub(r"\\\r?\n", "", self.cur_token.text)
        if len(self.cur_token.text) == len(line_joinFreeStringLiteral):
            self.add_pending_token(self.cur_token)
        else:
            originalSTRINGtoken: CommonToken = self.cur_token.clone() # backup the original token
            self.cur_token.text = line_joinFreeStringLiteral
            self.add_pending_token(self.cur_token)              # add the modified token with inline string literal
            self.hide_and_add_pending_token(originalSTRINGtoken) # add the original token to the hidden channel
            # this inserted hidden token allows to restore the original string literal with the \<newline> escape sequences

    def insert_trailing_tokens(self):
        match self.last_pending_token_type_from_default_channel:
            case self.NEWLINE | self.DEDENT:
                pass # no trailing NEWLINE token is needed
            case other:
                # insert an extra trailing NEWLINE token that serves as the end of the last statement
                self.create_and_add_pending_token(self.NEWLINE, Token.DEFAULT_CHANNEL, None, self.ffg_token) # _ffg_token is EOF
        self.insert_indent_or_dedent_token(0) # Now insert as much trailing DEDENT tokens as needed

    def handle_EOF_token(self):
        if self.last_pending_token_type_from_default_channel > 0:
            # there was statement in the input (leading NEWLINE tokens are hidden)
            self.insert_trailing_tokens()
        self.add_pending_token(self.cur_token)

    def hide_and_add_pending_token(self, cToken: CommonToken):
        cToken.channel = Token.HIDDEN_CHANNEL
        self.add_pending_token(cToken)

    def create_and_add_pending_token(self, type: int, channel: int, text: str, base_token: CommonToken):
        cToken: CommonToken = base_token.clone()
        cToken.type  = type
        cToken.channel = channel
        cToken.stop = base_token.start - 1
        cToken.text = "<" + self.symbolicNames[type] + ">" if text is None else \
                     text

        self.add_pending_token(cToken)

    def add_pending_token(self, token: CommonToken):
        # save the last pending token type because the _pending_tokens list can be empty by the nextToken()
        self.previous_pending_token_type = token.type
        if token.channel == Token.DEFAULT_CHANNEL:
            self.last_pending_token_type_from_default_channel = self.previous_pending_token_type
        self.pending_tokens.append(token)

    def get_indentation_length(self, textWS: str) -> int: # the textWS may contain spaces, tabs or form feeds
        TAB_LENGTH: int = 8 # the standard number of spaces to replace a tab to spaces
        length: int = 0
        ch: str
        for ch in textWS:
            match ch:
                case ' ':
                    self.was_space_indentation = True
                    length += 1
                case '\t':
                    self.was_tab_indentation = True
                    length += TAB_LENGTH - (length % TAB_LENGTH)
                case '\f': # form feed
                    length = 0

        if self.was_tab_indentation and self.was_space_indentation:
            if not self.was_indentation_mixed_with_spaces_and_tabs:
                self.was_indentation_mixed_with_spaces_and_tabs = True
                return self.INVALID_LENGTH # only for the first inconsistent indent
        return length

    def report_lexer_error(self, err_msg):
        self.getErrorListenerDispatch().syntaxError(self, self.cur_token, self.cur_token.line, self.cur_token.column, " LEXER" + self.ERR_TXT + err_msg, None)

    def report_error(self, err_msg):
        self.report_lexer_error(err_msg)

        # the ERROR_TOKEN will raise an error in the parser
        self.create_and_add_pending_token(self.ERROR_TOKEN, Token.DEFAULT_CHANNEL, self.ERR_TXT + err_msg, self.ffg_token)

    def reset(self):
        self.init()
        super().reset()
