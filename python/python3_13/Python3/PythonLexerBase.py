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

from typing import TextIO, Optional, List, Deque
from antlr4 import InputStream, Lexer, Token
from antlr4.Token import CommonToken
import sys
import re

class PythonLexerBase(Lexer):
    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

        # A stack that keeps track of the indentation lengths
        self.__indent_length_stack: List[int]

        # A list where tokens are waiting to be loaded into the token stream
        self.__pending_tokens: Deque[CommonToken]

        # last pending token type
        self.__previous_pending_token_type: int
        self.__last_pending_token_type_from_default_channel: int

        # The amount of opened parentheses, square brackets or curly braces
        self.__opened: int
        # The amount of opened parentheses and square brackets in the current lexer mode
        self.__paren_or_bracket_opened_stack: List[int]
        # A stack that stores expression(s) between braces in fstring
        self.__brace_expression_stack: List[str]
        self.__prev_brace_expression: str
        
        # Instead of self._mode      (self._mode is not implemented in each ANTLR4 runtime)
        self.__cur_lexer_mode: int
        # Instead of self._modeStack (self._modeStack is not implemented in each ANTLR4 runtime)
        self.__lexer_mode_stack: List[int]

        self.__was_space_indentation: bool
        self.__was_tab_indentation: bool
        self.__was_indentation_mixed_with_spaces_and_tabs: bool

        self.__cur_token: CommonToken # current (under processing) token
        self.__ffg_token: CommonToken # following (look ahead) token

        self.__INVALID_LENGTH: int = -1
        self.__ERR_TXT: str = " ERROR: "

        self.__init()

    def nextToken(self) -> CommonToken: # reading the input stream until a return EOF
        self.__check_next_token()
        return self.__pending_tokens.popleft() # add the queued token to the token stream

    def reset(self) -> None:
        self.__init()
        super().reset()

    def __init(self) -> None:
        self.__indent_length_stack = []
        self.__pending_tokens = Deque()
        self.__previous_pending_token_type = 0
        self.__last_pending_token_type_from_default_channel = 0
        self.__opened = 0
        self.__paren_or_bracket_opened_stack = []
        self.__brace_expression_stack = []
        self.__prev_brace_expression = ""
        self.__cur_lexer_mode = 0
        self.__lexer_mode_stack = []
        self.__was_space_indentation = False
        self.__was_tab_indentation = False
        self.__was_indentation_mixed_with_spaces_and_tabs = False
        self.__cur_token = None
        self.__ffg_token = None

    def __check_next_token(self) -> None:
        if self.__previous_pending_token_type == Token.EOF:
            return

        if not self.__indent_length_stack: # We're at the first token
            self.__insert_ENCODING_token()
            self.__set_current_and_following_tokens()
            self.__handle_start_of_input()
        else:
            self.__set_current_and_following_tokens()

        match self.__cur_token.type:
            case self.NEWLINE:
                self.__handle_NEWLINE_token()                
            case self.LPAR | self.LSQB | self.LBRACE:
                self.__opened += 1
                self.__add_pending_token(self.__cur_token)
            case self.RPAR | self.RSQB | self.RBRACE:
                self.__opened -= 1
                self.__add_pending_token(self.__cur_token)
            case self.FSTRING_MIDDLE:
                self.__handle_FSTRING_MIDDLE_token_with_double_brace() # does not affect the opened field
                self.__add_pending_token(self.__cur_token)
            case self.COLONEQUAL:
                self.__handle_COLONEQUAL_token_in_fstring()
            case self.ERRORTOKEN:
                self.__report_lexer_error("token recognition error at: '" + self.__cur_token.text + "'")
                self.__add_pending_token(self.__cur_token)
            case Token.EOF:
                self.__handle_EOF_token()
            case _:
                self.__add_pending_token(self.__cur_token)
        self.__handle_FORMAT_SPECIFICATION_MODE()

    def __set_current_and_following_tokens(self) -> None:
        self.__cur_token = super().nextToken() if self.__ffg_token is None else \
                           self.__ffg_token

        self.__check_cur_token() # ffgToken cannot be used in this method and its sub methods (ffgToken is not yet set)!
        
        self.__ffg_token = self.__cur_token if self.__cur_token.type == Token.EOF else \
                           super().nextToken()

    def __insert_ENCODING_token(self) -> None:  # https://peps.python.org/pep-0263/
        line_builder: list[str] = []
        encoding_name: str = ""
        line_count: int = 0
        ws_comment_pattern: re.Pattern = re.compile(r"^[ \t\f]*(#.*)?$")
        input_stream: InputStream = self.inputStream
        size: int = input_stream.size

        input_stream.seek(0)
        for i in range(size):
            c: str = chr(input_stream.LA(i + 1))
            line_builder.append(c)

            if c == '\n' or i == size - 1:
                line: str = ''.join(line_builder).replace("\r", "").replace("\n", "")
                if ws_comment_pattern.match(line): # WS* + COMMENT? found
                    encoding_name = self.__get_encoding_name(line)
                    if encoding_name:
                        break # encoding found
                else:
                    break # statement or backslash found (first line is not empty, not whitespace(s), not comment)
                
                line_count += 1
                if line_count >= 2:
                    break # check only the first two lines
                line_builder = []

        if not encoding_name:
            encoding_name = "utf-8"  # default Python source code encoding

        encoding_token: CommonToken = CommonToken((None, None), self.ENCODING, CommonToken.HIDDEN_CHANNEL, 0, 0)
        encoding_token.text = encoding_name
        encoding_token.line = 0
        encoding_token.column = -1
        self.__add_pending_token(encoding_token)

    def __get_encoding_name(self, comment_text: str) -> str:  # https://peps.python.org/pep-0263/#defining-the-encoding
        encoding_comment_pattern: str = r"^[ \t\f]*#.*?coding[:=][ \t]*([-_.a-zA-Z0-9]+)"
        match: Optional[re.Match] = re.search(encoding_comment_pattern, comment_text)
        return match.group(1) if match else ""

    # initialize the _indent_length_stack
    # hide the leading NEWLINE token(s)
    # if exists, find the first statement (not NEWLINE, not EOF token) that comes from the default channel
    # insert a leading INDENT token if necessary
    def __handle_start_of_input(self) -> None:
        # initialize the stack with a default 0 indentation length
        self.__indent_length_stack.append(0) # this will never be popped off
        while self.__cur_token.type != Token.EOF:
            if self.__cur_token.channel == Token.DEFAULT_CHANNEL:
                if self.__cur_token.type == self.NEWLINE:
                    # all the NEWLINE tokens must be ignored before the first statement
                    self.__hide_and_add_pending_token(self.__cur_token)
                else: # We're at the first statement
                    self.__insert_leading_indent_token()
                    return # continue the processing of the current token with __check_next_token()
            else:
                self.__add_pending_token(self.__cur_token) # it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            self.__set_current_and_following_tokens()
        # continue the processing of the EOF token with __check_next_token()

    def __insert_leading_indent_token(self) -> None:
        if self.__previous_pending_token_type == self.WS:
            prev_token: CommonToken = self.__pending_tokens[-1]  # WS token
            if self.__get_indentation_length(prev_token.text) != 0: # there is an "indentation" before the first statement
                err_msg: str = "first statement indented"
                self.__report_lexer_error(err_msg)
                # insert an INDENT token before the first statement to raise an 'unexpected indent' error later by the parser
                self.__create_and_add_pending_token(self.INDENT, Token.DEFAULT_CHANNEL, self.__ERR_TXT + err_msg, self.__cur_token)

    def __handle_NEWLINE_token(self) -> None:
        if self.__lexer_mode_stack: # not is_empty
            self.__add_pending_token(self.__cur_token)
        elif self.__opened > 0: # We're in an implicit line joining, ignore the current NEWLINE token
            self.__hide_and_add_pending_token(self.__cur_token)
        else:
            nl_token: CommonToken = self.__cur_token.clone() # save the current NEWLINE token
            is_looking_ahead: bool = self.__ffg_token.type == self.WS
            if is_looking_ahead:
                self.__set_current_and_following_tokens() # set the next two tokens

            match self.__ffg_token.type:
                case self.NEWLINE | self.COMMENT:
                    # We're before a blank line or a comment or type comment or a type ignore comment
                    self.__hide_and_add_pending_token(nl_token) # ignore the NEWLINE token
                    if is_looking_ahead:
                        self.__add_pending_token(self.__cur_token) # WS token
                case _:
                    self.__add_pending_token(nl_token)
                    if is_looking_ahead: # We're on a whitespace(s) followed by a statement
                        indentation_length: int = 0 if self.__ffg_token.type == Token.EOF else \
                                                  self.__get_indentation_length(self.__cur_token.text)

                        if indentation_length != self.__INVALID_LENGTH:
                            self.__add_pending_token(self.__cur_token) # WS token
                            self.__insert_INDENT_or_DEDENT_token(indentation_length) # may insert INDENT token or DEDENT token(s)
                        else:
                            self.__report_error("inconsistent use of tabs and spaces in indentation")
                    else: # We're at a newline followed by a statement (there is no whitespace before the statement)
                        self.__insert_INDENT_or_DEDENT_token(0) # may insert DEDENT token(s)

    def __insert_INDENT_or_DEDENT_token(self, indent_length: int) -> None:
        prev_indent_length: int = self.__indent_length_stack[-1] # stack peek
        if indent_length > prev_indent_length:
            self.__create_and_add_pending_token(self.INDENT, Token.DEFAULT_CHANNEL, None, self.__ffg_token)
            self.__indent_length_stack.append(indent_length) # stack push
        else:
            while indent_length < prev_indent_length: # more than 1 DEDENT token may be inserted to the token stream
                self.__indent_length_stack.pop()
                prev_indent_length = self.__indent_length_stack[-1] # stack peek
                if indent_length <= prev_indent_length:
                    self.__create_and_add_pending_token(self.DEDENT, Token.DEFAULT_CHANNEL, None, self.__ffg_token)
                else:
                    self.__report_error("inconsistent dedent")

    def __check_cur_token(self) -> None:
        match self.__cur_token.type:
            case self.FSTRING_START:
                self.__set_lexer_mode_by_FSTRING_START_token()
                return
            case self.FSTRING_MIDDLE:
                self.__handle_FSTRING_MIDDLE_token_with_quote_and_lbrace() # affect the opened field
                if self.__cur_token.type == self.FSTRING_MIDDLE:
                    return  # No __cur_token exchange happened
            case self.FSTRING_END:
                self.__pop_lexer_mode()
                return
            case _:
                if not self.__lexer_mode_stack:
                    return  # Not in fstring mode

        match self.__cur_token.type:  # the following tokens can only come from default mode (after an LBRACE in fstring)
            case self.NEWLINE:
                # append the current brace expression with the current newline
                self.__append_to_brace_expression(self.__cur_token.text)
                self.__cur_token.channel = Token.HIDDEN_CHANNEL
            case self.LBRACE:
                # the outermost brace expression cannot be a dictionary comprehension or a set comprehension
                self.__brace_expression_stack.append("{")
                self.__paren_or_bracket_opened_stack.append(0) # stack push
                self.__push_lexer_mode(Lexer.DEFAULT_MODE)
            case self.LPAR | self.LSQB:
                # append the current brace expression with a "(" or a "["
                self.__append_to_brace_expression(self.__cur_token.text)
                # https://peps.python.org/pep-0498/#lambdas-inside-expressions
                self.__increment_brace_stack()
            case self.RPAR | self.RSQB:
                # append the current brace expression with a ")" or a "]"
                self.__append_to_brace_expression(self.__cur_token.text)
                self.__decrement_brace_stack()
            case self.COLON | self.COLONEQUAL:
                # append the current brace expression with a ":" or a ":="
                self.__append_to_brace_expression(self.__cur_token.text)
                self.__set_lexer_mode_by_COLON_or_COLONEQUAL_token()
            case self.RBRACE:
                self.__set_lexer_mode_after_RBRACE_token()
            case _:
                # append the current brace expression with the current token text
                self.__append_to_brace_expression(self.__cur_token.text)

    def __append_to_brace_expression(self, text: str) -> None:
        self.__brace_expression_stack[-1] += text

    def __increment_brace_stack(self) -> None: # increment the last element (peek() + 1)
        self.__paren_or_bracket_opened_stack[-1] += 1

    def __decrement_brace_stack(self) -> None: # decrement the last element (peek() - 1)
        self.__paren_or_bracket_opened_stack[-1] -= 1

    def __set_lexer_mode_after_RBRACE_token(self) -> None:
        match self.__cur_lexer_mode:
            case Lexer.DEFAULT_MODE:
                self.__pop_lexer_mode() # only once
                self.__pop_by_RBRACE()

            case self.SQ1__FORMAT_SPECIFICATION_MODE \
                | self.SQ1R_FORMAT_SPECIFICATION_MODE \
                | self.DQ1__FORMAT_SPECIFICATION_MODE \
                | self.DQ1R_FORMAT_SPECIFICATION_MODE \
                | self.SQ3__FORMAT_SPECIFICATION_MODE \
                | self.SQ3R_FORMAT_SPECIFICATION_MODE \
                | self.DQ3__FORMAT_SPECIFICATION_MODE \
                | self.DQ3R_FORMAT_SPECIFICATION_MODE:

                self.__pop_lexer_mode()
                self.__pop_lexer_mode()
                self.__pop_by_RBRACE()
            case _:
                self.__report_lexer_error("f-string: single '}' is not allowed")

    def __set_lexer_mode_by_FSTRING_START_token(self) -> None:
        text = self.__cur_token.text.lower()
        mode_map = {
            "f'": self.SQ1__FSTRING_MODE,
            "rf'": self.SQ1R_FSTRING_MODE,
            "fr'": self.SQ1R_FSTRING_MODE,
            'f"': self.DQ1__FSTRING_MODE,
            'rf"': self.DQ1R_FSTRING_MODE,
            'fr"': self.DQ1R_FSTRING_MODE,
            "f'''": self.SQ3__FSTRING_MODE,
            "rf'''": self.SQ3R_FSTRING_MODE,
            "fr'''": self.SQ3R_FSTRING_MODE,
            'f"""': self.DQ3__FSTRING_MODE,
            'rf"""': self.DQ3R_FSTRING_MODE,
            'fr"""': self.DQ3R_FSTRING_MODE,
        }
        mode = mode_map.get(text)
        if mode is not None:
            self.__push_lexer_mode(mode)

    def __set_lexer_mode_by_COLON_or_COLONEQUAL_token(self) -> None:
        if self.__paren_or_bracket_opened_stack[-1] == 0: # stack peek == 0
            # COLONEQUAL token will be replaced with a COLON token in checkNextToken()
            match self.__lexer_mode_stack[-1]: # check the previous lexer mode (the current is DEFAULT_MODE)
                case self.SQ1__FSTRING_MODE \
                    | self.SQ1__FORMAT_SPECIFICATION_MODE:

                    self.__push_lexer_mode(self.SQ1__FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                case self.SQ1R_FSTRING_MODE \
                    | self.SQ1R_FORMAT_SPECIFICATION_MODE:

                    self.__push_lexer_mode(self.SQ1R_FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                case self.DQ1__FSTRING_MODE \
                    | self.DQ1__FORMAT_SPECIFICATION_MODE:

                    self.__push_lexer_mode(self.DQ1__FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                case self.DQ1R_FSTRING_MODE \
                    | self.DQ1R_FORMAT_SPECIFICATION_MODE:

                    self.__push_lexer_mode(self.DQ1R_FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                case self.SQ3__FSTRING_MODE \
                    | self.SQ3__FORMAT_SPECIFICATION_MODE:
                    
                    self.__push_lexer_mode(self.SQ3__FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                case self.SQ3R_FSTRING_MODE \
                    | self.SQ3R_FORMAT_SPECIFICATION_MODE:
                    
                    self.__push_lexer_mode(self.SQ3R_FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                case self.DQ3__FSTRING_MODE \
                    | self.DQ3__FORMAT_SPECIFICATION_MODE:
                    
                    self.__push_lexer_mode(self.DQ3__FORMAT_SPECIFICATION_MODE) # continue in format spec. mode
                case self.DQ3R_FSTRING_MODE \
                    | self.DQ3R_FORMAT_SPECIFICATION_MODE:
                    
                    self.__push_lexer_mode(self.DQ3R_FORMAT_SPECIFICATION_MODE) # continue in format spec. mode

    def __pop_by_RBRACE(self) -> None:
        self.__paren_or_bracket_opened_stack.pop()
        self.__prev_brace_expression = self.__brace_expression_stack.pop() + "}"
        if self.__brace_expression_stack:
            # append the current brace expression with the previous brace expression
            self.__brace_expression_stack[-1] += self.__prev_brace_expression

    def __handle_FSTRING_MIDDLE_token_with_double_brace(self) -> None:
        # replace the trailing double brace with a single brace and insert a hidden brace token
        match self.__get_last_two_chars_of_the_cur_token_text():
            case "{{":
                self.__trim_last_char_add_pending_token_set_cur_token(self.LBRACE, "{", Token.HIDDEN_CHANNEL)
            case "}}":
                self.__trim_last_char_add_pending_token_set_cur_token(self.RBRACE, "}", Token.HIDDEN_CHANNEL)

    def __handle_FSTRING_MIDDLE_token_with_quote_and_lbrace(self) -> None:
        # replace the trailing     quote + left_brace with a quote     and insert an LBRACE token
        # replace the trailing backslash + left_brace with a backslash and insert an LBRACE token        
        match self.__get_last_two_chars_of_the_cur_token_text():
            case "\"{" | "'{" | "\\{":
                self.__trim_last_char_add_pending_token_set_cur_token(self.LBRACE, "{", Token.DEFAULT_CHANNEL)

    def __get_last_two_chars_of_the_cur_token_text(self) -> str:
        cur_token_text: str = self.__cur_token.text
        return cur_token_text[-2:] if len(cur_token_text) >= 2 else cur_token_text

    def __trim_last_char_add_pending_token_set_cur_token(self, type: int, text: str, channel: int) -> None:
        # trim the last char and add the modified curToken to the __pending_tokens stack
        token_text_without_lbrace: str = self.__cur_token.text[:-1]
        self.__cur_token.text = token_text_without_lbrace
        self.__cur_token.stop -= 1
        self.__add_pending_token(self.__cur_token)

        self.__create_new_cur_token(type, text, channel) # set __cur_token

    def __handle_COLONEQUAL_token_in_fstring(self) -> None:
        if self.__lexer_mode_stack \
           and self.__paren_or_bracket_opened_stack[-1] == 0: # stack peek == 0

            # In fstring a colonequal (walrus operator) can only be used in parentheses
            # Not in parentheses, replace COLONEQUAL token with COLON as format specifier
            # and insert the equal symbol to the following FSTRING_MIDDLE token
            self.__cur_token.type = self.COLON
            self.__cur_token.text = ":"
            self.__cur_token.stop = self.__cur_token.start
            if self.__ffg_token.type == self.FSTRING_MIDDLE:
                self.__ffg_token.text = "=" + self.__ffg_token.text
                self.__ffg_token.start -= 1
                self.__ffg_token.column -= 1
            else:
                self.__add_pending_token(self.__cur_token)
                self.__create_new_current_token(self.FSTRING_MIDDLE, "=", Token.DEFAULT_CHANNEL)
        self.__add_pending_token(self.__cur_token)

    def __create_new_cur_token(self, type: int, text: str, channel: int) -> None:
        ctkn: CommonToken  = self.__cur_token.clone()
        ctkn.type  = type
        ctkn.text = text
        ctkn.channel = channel
        ctkn.column += 1
        ctkn.start += 1
        ctkn.stop = ctkn.start
        self.__cur_token = ctkn

    def __push_lexer_mode(self, mode: int) -> None:
        self.pushMode(mode)
        self.__lexer_mode_stack.append(self.__cur_lexer_mode) # stack push
        self.__cur_lexer_mode = mode

    def __pop_lexer_mode(self) -> None:
        self.popMode()
        self.__cur_lexer_mode = self.__lexer_mode_stack.pop()

    def __handle_FORMAT_SPECIFICATION_MODE(self) -> None:
        if self.__lexer_mode_stack \
           and self.__ffg_token.type == self.RBRACE:

            # insert an empty FSTRING_MIDDLE token instead of the missing format specification
            match self.__cur_token.type:
                case self.COLON:
                    self.__create_and_add_pending_token(self.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", self.__ffg_token)
                case self.RBRACE:
                    # only if the previous brace expression is not a dictionary comprehension or set comprehension
                    if not self.__is_dictionary_comprehension_or_set_comprehension(self.__prev_brace_expression):
                        self.__create_and_add_pending_token(self.FSTRING_MIDDLE, Token.DEFAULT_CHANNEL, "", self.__ffg_token)

    def __is_dictionary_comprehension_or_set_comprehension(self, code: str) -> bool:
        from antlr4 import InputStream, CommonTokenStream
        from PythonLexer import PythonLexer
        from PythonParser import PythonParser

        input_stream: InputStream = InputStream(code)
        lexer: PythonLexer = PythonLexer(input_stream)
        token_stream: CommonTokenStream  = CommonTokenStream(lexer)
        parser: PythonParser = PythonParser(token_stream)

        # Disable error listeners to suppress console output
        lexer.removeErrorListeners()  
        parser.removeErrorListeners()

        parser.dictcomp() # Try parsing as dictionary comprehension
        if parser.getNumberOfSyntaxErrors() == 0:
            return True

        parser = PythonParser(token_stream)
        token_stream.seek(0)
        parser.removeErrorListeners()
        parser.setcomp() # Try parsing as set comprehension
        return parser.getNumberOfSyntaxErrors() == 0

    def __insert_trailing_tokens(self) -> None:
        match self.__last_pending_token_type_from_default_channel:
            case self.NEWLINE | self.DEDENT:
                pass # no trailing NEWLINE token is needed
            case _: # insert an extra trailing NEWLINE token that serves as the end of the last statement
                self.__create_and_add_pending_token(self.NEWLINE, Token.DEFAULT_CHANNEL, None, self.__ffg_token) # _ffg_token is EOF
        self.__insert_INDENT_or_DEDENT_token(0) # Now insert as much trailing DEDENT tokens as needed

    def __handle_EOF_token(self) -> None:
        if self.__last_pending_token_type_from_default_channel > 0:
            # there was statement in the input (leading NEWLINE tokens are hidden)
            self.__insert_trailing_tokens()
        self.__add_pending_token(self.__cur_token)

    def __hide_and_add_pending_token(self, ctkn: CommonToken) -> None:
        ctkn.channel = Token.HIDDEN_CHANNEL
        self.__add_pending_token(ctkn)

    def __create_and_add_pending_token(self, ttype: int, channel: int, text: Optional[str], sample_token: CommonToken) -> None:
        ctkn: CommonToken = sample_token.clone()
        ctkn.type  = ttype
        ctkn.channel = channel
        ctkn.stop = sample_token.start - 1
        ctkn.text = "<" + self.symbolicNames[ttype] + ">" if text is None else \
                    text

        self.__add_pending_token(ctkn)

    def __add_pending_token(self, ctkn: CommonToken) -> None:
        # save the last pending token type because the _pending_tokens list can be empty by the nextToken()
        self.__previous_pending_token_type = ctkn.type
        if ctkn.channel == Token.DEFAULT_CHANNEL:
            self.__last_pending_token_type_from_default_channel = self.__previous_pending_token_type
        self.__pending_tokens.append(ctkn)

    def __get_indentation_length(self, indentText: str) -> int: # the indentText may contain spaces, tabs or form feeds
        TAB_LENGTH: int = 8 # the standard number of spaces to replace a tab with spaces
        length: int = 0
        ch: str
        for ch in indentText:
            match ch:
                case ' ':
                    self.__was_space_indentation = True
                    length += 1
                case '\t':
                    self.__was_tab_indentation = True
                    length += TAB_LENGTH - (length % TAB_LENGTH)
                case '\f': # form feed
                    length = 0

        if self.__was_tab_indentation and self.__was_space_indentation:
            if not self.__was_indentation_mixed_with_spaces_and_tabs:
                self.__was_indentation_mixed_with_spaces_and_tabs = True
                length = self.__INVALID_LENGTH # only for the first inconsistent indent
        return length

    def __report_lexer_error(self, err_msg: str) -> None:
        self.getErrorListenerDispatch().syntaxError(self, self.__cur_token, self.__cur_token.line, self.__cur_token.column, " LEXER" + self.__ERR_TXT + err_msg, None)

    def __report_error(self, err_msg: str) -> None:
        self.__report_lexer_error(err_msg)

        # the ERRORTOKEN will raise an error in the parser
        self.__create_and_add_pending_token(self.ERRORTOKEN, Token.DEFAULT_CHANNEL, self.__ERR_TXT + err_msg, self.__ffg_token)
