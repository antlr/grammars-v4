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

from typing import TextIO
from antlr4 import InputStream, Lexer, Token
from antlr4.Token import CommonToken
import sys
import re

class PythonLexerBase(Lexer):
    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

        # A stack that keeps track of the indentation lengths
        self._indent_lengths: list[int] = []

        # A list where tokens are waiting to be loaded into the token stream
        self._pending_tokens: list[CommonToken] = []

        # last pending token types
        self._previous_pending_token_type: int = 0
        self._last_pending_token_type_for_default_channel: int = 0

        # The amount of opened parentheses, square brackets or curly braces
        self._opened: int = 0
        # The amount of opened parentheses and square brackets in the current lexer mode
        self._paren_or_bracket_opened: list[int] = []

        self._was_space_indentation: bool = False
        self._was_tab_indentation: bool = False
        self._was_indentation_mixed_with_spaces_and_tabs: bool = False
        self._INVALID_LENGTH: int = -1

        self._cur_token: CommonToken = None # current (under processing) token
        self._ffg_token: CommonToken = None # following (look ahead) token

        self._ERR_TXT: str = " ERROR: "

    def nextToken(self) -> CommonToken: # reading the input stream until a return EOF
        self.check_next_token()
        return self._pending_tokens.pop(0) # add the queued token to the token stream

    def check_next_token(self):
        if self._previous_pending_token_type != Token.EOF:
            self.set_current_and_following_tokens()
            if len(self._indent_lengths) == 0: # We're at the first token
                self.handle_start_of_input()
            match self._cur_token.type:
                case self.LPAR | self.LSQB | self.LBRACE:
                    self._opened += 1
                    self.add_pending_token(self._cur_token)
                case self.RPAR | self.RSQB | self.RBRACE:
                    self._opened -= 1
                    self.add_pending_token(self._cur_token)
                case self.NEWLINE:
                    self.handle_NEWLINE_token()
                case self.STRING:
                    self.handle_STRING_token()
                case self.FSTRING_MIDDLE:
                    self.handle_FSTRING_MIDDLE_token()
                case self.ERROR_TOKEN:
                    self.report_lexer_error("token recognition error at: '" + self._cur_token.text + "'")
                    self.add_pending_token(self._cur_token)
                case Token.EOF:
                    self.handle_EOF_token()
                case other:
                    self.add_pending_token(self._cur_token)
            self.handle_FORMAT_SPECIFICATION_MODE()

    def set_current_and_following_tokens(self):
        self._cur_token = super().nextToken() if self._ffg_token is None else \
                          self._ffg_token

        self.handle_fstring_lexer_modes()
        
        self._ffg_token = self._cur_token if self._cur_token.type == Token.EOF else \
                          super().nextToken()

    # initialize the _indent_lengths stack
    # hide the leading NEWLINE token(s)
    # if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    # insert a leading INDENT token if necessary
    def handle_start_of_input(self):
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
        if self._previous_pending_token_type == self.WS:
            prev_token: CommonToken = self._pending_tokens[-1]  # WS token
            if self.get_indentation_length(prev_token.text) != 0: # there is an "indentation" before the first statement
                err_msg: str = "first statement indented"
                self.report_lexer_error(err_msg)
                # insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                self.create_and_add_pending_token(self.INDENT, Token.DEFAULT_CHANNEL, self._ERR_TXT + err_msg, self._cur_token)

    def handle_NEWLINE_token(self):
        if self._opened > 0: # We're in an implicit line joining, ignore the current NEWLINE token
            self.hide_and_add_pending_token(self._cur_token)
        else:
            nl_token: CommonToken = self._cur_token # save the current NEWLINE token
            is_looking_ahead: bool = self._ffg_token.type == self.WS
            if is_looking_ahead:
                self.set_current_and_following_tokens() # set the two next tokens

            match self._ffg_token.type:
                case self.NEWLINE | self.COMMENT | self.TYPE_COMMENT:
                    # We're before a blank line or a comment or a type comment
                    self.hide_and_add_pending_token(nl_token)     # ignore the NEWLINE token
                    if is_looking_ahead:
                        self.add_pending_token(self._cur_token) # WS token
                case other:
                    self.add_pending_token(nl_token)
                    if is_looking_ahead: # We're on a whitespace(s) followed by a statement
                        indentation_length: int = 0 if self._ffg_token.type == Token.EOF else \
                                                  self.get_indentation_length(self._cur_token.text)

                        if indentation_length != self._INVALID_LENGTH:
                            self.add_pending_token(self._cur_token) # WS token
                            self.insert_indent_or_dedent_token(indentation_length) # may insert INDENT token or DEDENT token(s)
                        else:
                            self.report_error("inconsistent use of tabs and spaces in indentation")
                    else: # We're at a newline followed by a statement (there is no whitespace before the statement)
                        self.insert_indent_or_dedent_token(0) # may insert DEDENT token(s)

    def insert_indent_or_dedent_token(self, cur_indent_length: int):
        prev_indent_length: int = self._indent_lengths[-1]
        if cur_indent_length > prev_indent_length:
            self.create_and_add_pending_token(self.INDENT, Token.DEFAULT_CHANNEL, None, self._ffg_token)
            self._indent_lengths.append(cur_indent_length)
        else:
            while cur_indent_length < prev_indent_length: # more than 1 DEDENT token may be inserted to the token stream
                self._indent_lengths.pop()
                prev_indent_length = self._indent_lengths[-1]
                if cur_indent_length <= prev_indent_length:
                    self.create_and_add_pending_token(self.DEDENT, Token.DEFAULT_CHANNEL, None, self._ffg_token)
                else:
                    self.report_error("inconsistent dedent")

    def handle_STRING_token(self): # remove the \<newline> escape sequences from the string literal
        # https://docs.python.org/3.11/reference/lexical_analysis.html#string-and-bytes-literals
        line_joinFreeStringLiteral: str = re.sub(r"\\\r?\n", "", self._cur_token.text)
        if len(self._cur_token.text) == len(line_joinFreeStringLiteral):
            self.add_pending_token(self._cur_token)
        else:
            originalSTRINGtoken: CommonToken = self._cur_token.clone() # backup the original token
            self._cur_token.text = line_joinFreeStringLiteral
            self.add_pending_token(self._cur_token)              # add the modified token with inline string literal
            self.hide_and_add_pending_token(originalSTRINGtoken) # add the original token to the hidden channel
            # this inserted hidden token allows to restore the original string literal with the \<newline> escape sequences

    def handle_FSTRING_MIDDLE_token(self): # replace the double braces '{{' or '}}' to single braces and hide the second braces
            fs_mid: str = self._cur_token.text
            fs_mid = fs_mid.replace("{{", "{_").replace("}}", "}_") # replace: {{ --> {_    }} --> }_
            arrOfStr: list[str] = re.split(r"(?<=[{}])_", fs_mid) # split by {_  or  }_
            s: str
            for s in arrOfStr:
                if s:
                    self.create_and_add_pending_token(self.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, s, self._ffg_token)
                    lastCharacter: str = s[-1:]
                    if lastCharacter in "{}":
                        self.create_and_add_pending_token(self.FSTRING_MIDDLE, Token.HIDDEN_CHANNEL, lastCharacter, self._ffg_token)

    def handle_fstring_lexer_modes(self):
        if self._modeStack:
            match self._cur_token.type:
                case self.LBRACE:
                    self.pushMode(Lexer.DEFAULT_MODE)
                    self._paren_or_bracket_opened.append(0)
                case self.LPAR | self.LSQB:
                    # https://peps.python.org/pep-0498/#lambdas-inside-expressions
                    self._paren_or_bracket_opened[-1] += 1 # increment the last element
                case self.RPAR | self.RSQB:
                    self._paren_or_bracket_opened[-1] -= 1 # decrement the last element
                case self.COLON:
                    if self._paren_or_bracket_opened[-1] == 0:
                        match self._modeStack[-1]: # check the previous lexer mode (the current is DEFAULT_MODE)
                            case self.SINGLE_QUOTE_FSTRING_MODE \
                               | self.LONG_SINGLE_QUOTE_FSTRING_MODE \
                               | self.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE:

                                self.mode(self.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                            case self.DOUBLE_QUOTE_FSTRING_MODE \
                               | self.LONG_DOUBLE_QUOTE_FSTRING_MODE \
                               | self.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE:

                                self.mode(self.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                case self.RBRACE:
                    match self._mode:
                        case Lexer.DEFAULT_MODE \
                           | self.SINGLE_QUOTE_FORMAT_SPECIFICATION_MODE \
                           | self.DOUBLE_QUOTE_FORMAT_SPECIFICATION_MODE:

                            self.popMode()
                            self._paren_or_bracket_opened.pop()
                        case other:
                            self.report_lexer_error("f-string: single '}' is not allowed")

    def handle_FORMAT_SPECIFICATION_MODE(self):
        if len(self._modeStack) != 0 \
           and self._ffg_token.type == self.RBRACE:
            
            match self._cur_token.type:
                case self.COLON | self.RBRACE:
                    # insert an empty FSTRING_MIDDLE token instead of the missing format specification
                    self.create_and_add_pending_token(self.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", self._ffg_token)

    def insert_trailing_tokens(self):
        match self._last_pending_token_type_for_default_channel:
            case self.NEWLINE | self.DEDENT:
                pass # no trailing NEWLINE token is needed
            case other:
                # insert an extra trailing NEWLINE token that serves as the end of the last statement
                self.create_and_add_pending_token(self.NEWLINE, Token.DEFAULT_CHANNEL, None, self._ffg_token) # _ffg_token is EOF
        self.insert_indent_or_dedent_token(0) # Now insert as much trailing DEDENT tokens as needed

    def handle_EOF_token(self):
        if self._last_pending_token_type_for_default_channel > 0:
            # there was statement in the input (leading NEWLINE tokens are hidden)
            self.insert_trailing_tokens()
        self.add_pending_token(self._cur_token)

    def hide_and_add_pending_token(self, token: CommonToken):
        token.channel = Token.HIDDEN_CHANNEL
        self.add_pending_token(token)

    def create_and_add_pending_token(self, type: int, channel: int, text: str, base_token: CommonToken):
        token: CommonToken = base_token.clone()
        token.type  = type
        token.channel = channel
        token.stop = base_token.start - 1
        token.text = "<" + self.symbolicNames[type] + ">" if text is None else \
                     text

        self.add_pending_token(token)

    def add_pending_token(self, token: CommonToken):
        # save the last pending token type because the _pending_tokens list can be empty by the nextToken()
        self._previous_pending_token_type = token.type
        if token.channel == Token.DEFAULT_CHANNEL:
            self._last_pending_token_type_for_default_channel = self._previous_pending_token_type
        self._pending_tokens.append(token)

    def get_indentation_length(self, textWS: str) -> int: # the textWS may contain spaces, tabs or formfeeds
        TAB_LENGTH: int = 8 # the standard number of spaces to replace a tab to spaces
        length: int = 0
        ch: str
        for ch in textWS:
            match ch:
                case ' ':
                    self._was_space_indentation = True
                    length += 1
                case '\t':
                    self._was_tab_indentation = True
                    length += TAB_LENGTH - (length % TAB_LENGTH)
                case '\f': # formfeed
                    length = 0

        if self._was_tab_indentation and self._was_space_indentation:
            if not self._was_indentation_mixed_with_spaces_and_tabs:
                self._was_indentation_mixed_with_spaces_and_tabs = True
                return self._INVALID_LENGTH # only for the first inconsistent indent
        return length

    def report_lexer_error(self, err_msg):
        self.getErrorListenerDispatch().syntaxError(self, self._cur_token, self._cur_token.line, self._cur_token.column, self._ERR_TXT + err_msg, None)

    def report_error(self, err_msg):
        self.report_lexer_error(err_msg)

        # the ERROR_TOKEN will raise an error in the parser
        self.create_and_add_pending_token(self.ERROR_TOKEN, Token.DEFAULT_CHANNEL, self._ERR_TXT + err_msg, self._ffg_token)
