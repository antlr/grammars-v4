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

# Project      : A helper class for an ANTLR4 Python lexer grammar that assists in tokenizing indentation,
#                interpolated strings, and encoding declaration.
#
# Developed by : Robert Einhorn

from collections import deque
from typing import Literal, TextIO, Optional
from antlr4 import InputStream, Lexer, Token
from antlr4.Token import CommonToken
import PythonLexer
import sys

class PythonLexerBase(Lexer):
    INVALID_LENGTH: Literal[-1] = -1
    ERR_TXT: Literal[" ERROR: "] = " ERROR: "
    TAB_LENGTH: Literal[8] = 8    
    LEXER_MODES_FOR_ISTRING_START: dict[str, int] = {}

    def __init__(self, input: InputStream, output: TextIO = sys.stdout):
        super().__init__(input, output)
        self._init()

    def reset(self) -> None:
        self._init()
        super().reset()

    def _init(self) -> None:
        self._encodingName: str = ""

        # Indentation handling
        self._indentation_length_stack: list[int] = []
        self._pending_token_queue: deque[CommonToken] = deque()

        self._previous_pending_token_type: int = 0
        self._last_pending_token_type_from_default_channel = 0

        # Parenthesis / bracket / brace counts
        self._open_paren_bracket_brace_count: int = 0
        self._paren_or_bracket_opened_stack: list[int] = []
        self._brace_expression_stack: list[str] = []
        self._last_closed_brace_expression: str = ""

        # Current interpolated STRING_MIDDLE token type (FSTRING_MIDDLE or TSTRING_MIDDLE)
        self._active_interpolated_string_middle_token_type: int = 0

        # We reimplement mode/stack because not all runtimes expose _mode/_modeStack
        self._cur_lexer_mode: int = Lexer.DEFAULT_MODE
        self._lexer_mode_stack: list[int] = []

        # Indentation diagnostics
        self._was_space_indentation: bool = False
        self._was_tab_indentation: bool = False
        self._has_mixed_indentation_been_reported: bool = False

        # Current / lookahead tokens
        self._cur_token: CommonToken = None
        self._la_token: CommonToken = None

    def set_encoding_name(self, encoding_name: str) -> None:
        """
        Sets the encoding name to emit an ENCODING token at the start of the token stream.
        Leave empty if not needed (e.g., when parsing from string).

        :param encoding_name: The encoding name (e.g., "utf-8"), or empty string to disable ENCODING token.
        """
        self._encodingName = encoding_name

    def nextToken(self) -> CommonToken:  # Reading the input stream until EOF is reached
        self._process_current_token()
        return self._pending_token_queue.popleft()  # Add the queued token to the token stream

    def _process_current_token(self) -> None:
        if self._previous_pending_token_type == Token.EOF:
            return

        self._set_current_and_following_tokens()
        if not self._indentation_length_stack:  # We're at the first token
            self._handle_start_of_input()

        match self._cur_token.type:
            case self.NEWLINE:
                self._handle_NEWLINE_token()
            case self.LPAR | self.LSQB | self.LBRACE:
                self._open_paren_bracket_brace_count += 1
                self.add_pending_token(self._cur_token)
            case self.RPAR | self.RSQB | self.RBRACE:
                self._open_paren_bracket_brace_count -= 1
                self.add_pending_token(self._cur_token)
            case self.FSTRING_MIDDLE | self.TSTRING_MIDDLE:
                self._handle_ISTRING_MIDDLE_token_with_double_brace()  # does not affect the opened field
                self.add_pending_token(self._cur_token)
            case self.COLONEQUAL:
                self._handle_COLONEQUAL_token_in_istring()
            case self.ERRORTOKEN:
                self._report_lexer_error("token recognition error at: '" + self._cur_token.text + "'")
                self.add_pending_token(self._cur_token)
            case Token.EOF:
                self._handle_EOF_token()
            case _:
                self.add_pending_token(self._cur_token)
        self._handle_FORMAT_SPECIFICATION_MODE()

    def _set_current_and_following_tokens(self) -> None:
        self._cur_token = super().nextToken() if self._la_token is None else self._la_token

        self._normalize_cur_token()  # Do not use laToken in this method or any of its submethods — it hasn't been set yet!

        self._la_token = self._cur_token if self._cur_token.type == Token.EOF else super().nextToken()

    # ===================== Leading‑Token Preprocessing =====================
    # Handles BOM skipping, ENCODING token insertion, suppression of leading
    # NEWLINE tokens, and validation of the first INDENT before normal token
    # processing begins.

    def _handle_start_of_input(self) -> None:
        # - initialize indent stack with a default 0 indentation length
        # - skip BOM token
        # - insert ENCODING token (if any)
        # - hide leading NEWLINE(s)
        # - insert leading INDENT if first statement is indented
        self._indentation_length_stack.append(0)  # this will never be popped off

        if self._cur_token.type == self.BOM:
            self._set_current_and_following_tokens()
        self._insert_ENCODING_token()

        while self._cur_token.type != Token.EOF:
            if self._cur_token.channel == Token.DEFAULT_CHANNEL:
                if self._cur_token.type == self.NEWLINE:
                    # all the NEWLINE tokens must be ignored before the first statement
                    self._hide_and_add_pending_token(self._cur_token)
                else:  # We're at the first statement
                    self._insert_leading_indent_token()
                    return  # continue the processing of the current token with _process_current_token()
            else:
                self.add_pending_token(self._cur_token)  # it can be WS, EXPLICIT_LINE_JOINING or COMMENT token
            self._set_current_and_following_tokens()
        # continue the processing of the EOF token with _process_current_token()

    def _insert_ENCODING_token(self) -> None:  # https://peps.python.org/pep-0263/
        if not self._encodingName:
            return

        source_pair = self._tokenFactorySourcePair
        encoding_token: CommonToken = CommonToken(source_pair, self.ENCODING, Token.HIDDEN_CHANNEL, start=0, stop=0)
        encoding_token.text = self._encodingName
        encoding_token.line = 0
        encoding_token.column = -1
        self.add_pending_token(encoding_token)

    def _insert_leading_indent_token(self) -> None:
        if self._previous_pending_token_type == self.WS:
            prev_token: CommonToken = self._pending_token_queue[-1]  # stack peek, WS token
            if self._get_indentation_length(prev_token.text) != 0:  # there is an "indentation" before the first statement
                err_msg: str = "first statement indented"
                self._report_lexer_error(err_msg)
                # insert an INDENT token before the first statement to trigger an 'unexpected indent' error later in the parser
                self._create_and_add_pending_token(self.INDENT, PythonLexerBase.ERR_TXT + err_msg, self._cur_token)

    # ===================== Indentation Handling =====================
    # Processes NEWLINE tokens, computes indentation length from leading
    # whitespace, manages the INDENT/DEDENT stack, emits the appropriate
    # indentation tokens, and detects inconsistent mixing of tabs and spaces

    def _handle_NEWLINE_token(self) -> None:
        if self._lexer_mode_stack:  # for multi line f/t-string literals
            self.add_pending_token(self._cur_token)
            return

        if self._open_paren_bracket_brace_count > 0:  # We're in an implicit line joining, ignore the current NEWLINE token
            self._hide_and_add_pending_token(self._cur_token)
            return

        nl_token: CommonToken = self._cur_token.clone()  # save the current NEWLINE token
        is_looking_ahead: bool = self._la_token.type == self.WS
        if is_looking_ahead:
            self._set_current_and_following_tokens()  # set the next two tokens

        match self._la_token.type:
            case self.NEWLINE | self.COMMENT:
                # We're before a blank line or a comment or type comment or a type ignore comment
                self._hide_and_add_pending_token(nl_token)  # ignore the NEWLINE token
                if is_looking_ahead:
                    self.add_pending_token(self._cur_token)  # WS token
            case _:
                self.add_pending_token(nl_token)
                if is_looking_ahead:  # We're on a whitespace(s) followed by a statement
                    indentation_length: int = 0 if self._la_token.type == Token.EOF else \
                                                self._get_indentation_length(self._cur_token.text)

                    if indentation_length != PythonLexerBase.INVALID_LENGTH:
                        self.add_pending_token(self._cur_token)  # WS token
                        self._insert_INDENT_or_DEDENT_token(indentation_length)  # may insert INDENT token or DEDENT token(s)
                    else:
                        self._report_error("inconsistent use of tabs and spaces in indentation")
                else:  # We're at a newline followed by a statement (there is no whitespace before the statement)
                    self._insert_INDENT_or_DEDENT_token(0)  # may insert DEDENT token(s)

    def _insert_INDENT_or_DEDENT_token(self, indent_length: int) -> None:
        prev_indent_length: int = self._indentation_length_stack[-1]  # stack peek
        if indent_length > prev_indent_length:
            self._create_and_add_pending_token(self.INDENT, None, self._la_token)
            self._indentation_length_stack.append(indent_length)  # stack push
            return

        while indent_length < prev_indent_length:  # more than 1 DEDENT token may be inserted to the token stream
            self._indentation_length_stack.pop()
            prev_indent_length = self._indentation_length_stack[-1]  # stack peek
            if indent_length <= prev_indent_length:
                self._create_and_add_pending_token(self.DEDENT, None, self._la_token)
            else:
                self._report_error("inconsistent dedent")

    def _get_indentation_length(self, indentText: str) -> int:  # the indentText may contain spaces, tabs or form feeds
        length: int = 0
        ch: str
        for ch in indentText:
            match ch:
                case ' ':
                    self._was_space_indentation = True
                    length += 1
                case '\t':
                    self._was_tab_indentation = True
                    length += PythonLexerBase.TAB_LENGTH - (length % PythonLexerBase.TAB_LENGTH)
                case '\f':  # form feed
                    length = 0

        if self._was_tab_indentation and self._was_space_indentation:
            if not self._has_mixed_indentation_been_reported:
                self._has_mixed_indentation_been_reported = True
                length = PythonLexerBase.INVALID_LENGTH  # only for the first inconsistent indent
        return length

    # ===================== IString Processing =====================
    # Handles f- and t-string interpolation, including prefix detection,
    # ISTRING_MIDDLE token rewriting, escaped brace handling ({{ and }}),
    # quote+brace boundary cases, format-specifier mode transitions,
    # and brace-expression accumulation inside interpolated segments.

    def _normalize_cur_token(self) -> None:
        match self._cur_token.type:
            case self.FSTRING_START:
                self._active_interpolated_string_middle_token_type = self.FSTRING_MIDDLE
                self._set_lexer_mode_by_ISTRING_START_token()
                return
            case self.TSTRING_START:
                self._active_interpolated_string_middle_token_type = self.TSTRING_MIDDLE
                self._set_lexer_mode_by_ISTRING_START_token()
                return
            case self.FSTRING_MIDDLE | self.TSTRING_MIDDLE:
                self._handle_ISTRING_MIDDLE_token_with_quote_and_lbrace()  # affect the opened field
                match self._cur_token.type:
                    case self.FSTRING_MIDDLE | self.TSTRING_MIDDLE:
                        return  # No _cur_token exchange happened
            case self.FSTRING_END | self.TSTRING_END:
                self._pop_lexer_mode()
                return
            case _:
                if not self._lexer_mode_stack:
                    return  # Not in fstring mode
        self._process_brace_expression()

    def _process_brace_expression(self) -> None:
        match self._cur_token.type:  # the following tokens can only come from default mode (after an LBRACE in f/t-string)
            case self.NEWLINE:
                # append the current brace expression with the current newline
                self._append_to_brace_expression(self._cur_token.text)
                self._cur_token.channel = Token.HIDDEN_CHANNEL
            case self.LBRACE:
                # the outermost brace expression cannot be a dictionary comprehension or a set comprehension
                self._brace_expression_stack.append("{")
                self._paren_or_bracket_opened_stack.append(0)  # stack push
                self._push_lexer_mode(Lexer.DEFAULT_MODE)
            case self.LPAR | self.LSQB:
                # append the current brace expression with a "(" or a "["
                self._append_to_brace_expression(self._cur_token.text)
                # https://peps.python.org/pep-0498/#lambdas-inside-expressions
                self._increment_brace_depth()
            case self.RPAR | self.RSQB:
                # append the current brace expression with a ")" or a "]"
                self._append_to_brace_expression(self._cur_token.text)
                self._decrement_brace_depth()
            case self.COLON | self.COLONEQUAL:
                # append the current brace expression with a ":" or a ":="
                self._append_to_brace_expression(self._cur_token.text)
                self._set_lexer_mode_by_COLON_or_COLONEQUAL_token()
            case self.RBRACE:
                self._set_lexer_mode_after_RBRACE_token()
            case _:
                # append the current brace expression with the current token text
                self._append_to_brace_expression(self._cur_token.text)

    def _append_to_brace_expression(self, text: str) -> None:
        self._brace_expression_stack[-1] += text

    def _increment_brace_depth(self) -> None:  # increment the last element (stack peek + 1)
        self._paren_or_bracket_opened_stack[-1] += 1

    def _decrement_brace_depth(self) -> None:  # decrement the last element (stack peek - 1)
        self._paren_or_bracket_opened_stack[-1] -= 1

    def _set_lexer_mode_after_RBRACE_token(self) -> None:
        match self._cur_lexer_mode:
            case Lexer.DEFAULT_MODE:
                self._pop_lexer_mode()  # only once
                self._pop_by_RBRACE()
            case (
                self.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE
                | self.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE
                | self.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE
                | self.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE
                | self.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE
                | self.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE
                | self.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE
                | self.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE
                | self.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE
                | self.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE
                | self.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE
                | self.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE
                | self.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE
                | self.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE
                | self.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE
                | self.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE
            ):

                self._pop_lexer_mode()
                self._pop_lexer_mode()
                self._pop_by_RBRACE()
            case _:
                self._report_lexer_error("f-string: single '}' is not allowed")

    def _set_lexer_mode_by_ISTRING_START_token(self) -> None: # ISTRING = interpolated string (FSTRING or TSTRING)
        if not PythonLexerBase.LEXER_MODES_FOR_ISTRING_START:
            self._init_lexer_modes_for_ISTRING_START()

        interpolated_string_prefix: str = self._cur_token.text.lower()
        if interpolated_string_prefix in PythonLexerBase.LEXER_MODES_FOR_ISTRING_START:
            new_lexer_mode: int = PythonLexerBase.LEXER_MODES_FOR_ISTRING_START[interpolated_string_prefix]
            self._push_lexer_mode(new_lexer_mode)
        else:
            self._report_lexer_error(
                f"internal error: unknown interpolated string literal prefix: {self._cur_token.text}"
            )

    def _init_lexer_modes_for_ISTRING_START(self) -> None:
        m = PythonLexerBase.LEXER_MODES_FOR_ISTRING_START

        # f-strings
        m["f'"] = self.SQ1__FSTRING_MODE
        m["rf'"] = self.SQ1R_FSTRING_MODE
        m["fr'"] = self.SQ1R_FSTRING_MODE
        m['f"'] = self.DQ1__FSTRING_MODE
        m['rf"'] = self.DQ1R_FSTRING_MODE
        m['fr"'] = self.DQ1R_FSTRING_MODE
        m["f'''"] = self.SQ3__FSTRING_MODE
        m["rf'''"] = self.SQ3R_FSTRING_MODE
        m["fr'''"] = self.SQ3R_FSTRING_MODE
        m['f"""'] = self.DQ3__FSTRING_MODE
        m['rf"""'] = self.DQ3R_FSTRING_MODE
        m['fr"""'] = self.DQ3R_FSTRING_MODE

        # t-strings
        m["t'"] = self.SQ1__TSTRING_MODE
        m["rt'"] = self.SQ1R_TSTRING_MODE
        m["tr'"] = self.SQ1R_TSTRING_MODE
        m['t"'] = self.DQ1__TSTRING_MODE
        m['rt"'] = self.DQ1R_TSTRING_MODE
        m['tr"'] = self.DQ1R_TSTRING_MODE
        m["t'''"] = self.SQ3__TSTRING_MODE
        m["rt'''"] = self.SQ3R_TSTRING_MODE
        m["tr'''"] = self.SQ3R_TSTRING_MODE
        m['t"""'] = self.DQ3__TSTRING_MODE
        m['rt"""'] = self.DQ3R_TSTRING_MODE
        m['tr"""'] = self.DQ3R_TSTRING_MODE

    def _set_lexer_mode_by_COLON_or_COLONEQUAL_token(self) -> None:
        # Exit early when the current lexer mode indicates an open parenthesis/bracket
        opened: bool = self._paren_or_bracket_opened_stack[-1] > 0  # stack peek
        if opened:
            return

        # COLONEQUAL token will be replaced with a COLON token in _process_current_token()
        prevLexerMode = self._lexer_mode_stack[-1]  # stack peek
        match prevLexerMode:
            case self.SQ1__FSTRING_MODE | self.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.SQ1__FSTRING_FORMAT_SPECIFICATION_MODE)

            case self.SQ1__TSTRING_MODE | self.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.SQ1__TSTRING_FORMAT_SPECIFICATION_MODE)

            case self.SQ1R_FSTRING_MODE | self.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.SQ1R_FSTRING_FORMAT_SPECIFICATION_MODE)

            case self.SQ1R_TSTRING_MODE | self.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.SQ1R_TSTRING_FORMAT_SPECIFICATION_MODE)

            case self.DQ1__FSTRING_MODE | self.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.DQ1__FSTRING_FORMAT_SPECIFICATION_MODE)

            case self.DQ1__TSTRING_MODE | self.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.DQ1__TSTRING_FORMAT_SPECIFICATION_MODE)

            case self.DQ1R_FSTRING_MODE | self.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.DQ1R_FSTRING_FORMAT_SPECIFICATION_MODE)

            case self.DQ1R_TSTRING_MODE | self.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.DQ1R_TSTRING_FORMAT_SPECIFICATION_MODE)

            case self.SQ3__FSTRING_MODE | self.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.SQ3__FSTRING_FORMAT_SPECIFICATION_MODE)
            case self.SQ3__TSTRING_MODE | self.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.SQ3__TSTRING_FORMAT_SPECIFICATION_MODE)

            case self.SQ3R_FSTRING_MODE | self.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.SQ3R_FSTRING_FORMAT_SPECIFICATION_MODE)
            case self.SQ3R_TSTRING_MODE | self.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.SQ3R_TSTRING_FORMAT_SPECIFICATION_MODE)

            case self.DQ3__FSTRING_MODE | self.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.DQ3__FSTRING_FORMAT_SPECIFICATION_MODE)

            case self.DQ3__TSTRING_MODE | self.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.DQ3__TSTRING_FORMAT_SPECIFICATION_MODE)
            case self.DQ3R_FSTRING_MODE | self.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.DQ3R_FSTRING_FORMAT_SPECIFICATION_MODE)

            case self.DQ3R_TSTRING_MODE | self.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE:
                self._push_lexer_mode(self.DQ3R_TSTRING_FORMAT_SPECIFICATION_MODE)

    def _pop_by_RBRACE(self) -> None:
        self._paren_or_bracket_opened_stack.pop()
        cur_brace_expression: str = self._brace_expression_stack.pop()
        self._last_closed_brace_expression = cur_brace_expression + "}"
        if self._brace_expression_stack:
            # Extend the current brace expression by adding the previous expression
            self._brace_expression_stack[-1] += self._last_closed_brace_expression

    def _handle_ISTRING_MIDDLE_token_with_double_brace(self) -> None:
        # ISTRING = interpolated string (FSTRING or TSTRING)
        last_two_chars: str = self._get_last_two_chars_of_the_cur_token_text()
        match last_two_chars:
            case "{{":
                self._trim_last_char_add_pending_token_set_cur_token(self.LBRACE, "{", Token.HIDDEN_CHANNEL)
            case "}}":
                self._trim_last_char_add_pending_token_set_cur_token(self.RBRACE, "}", Token.HIDDEN_CHANNEL)

    def _handle_ISTRING_MIDDLE_token_with_quote_and_lbrace(self) -> None:  # ISTRING = interpolated string (FSTRING or TSTRING)
        # replace the trailing     quote + left_brace with a quote     and insert an LBRACE token
        # replace the trailing backslash + left_brace with a backslash and insert an LBRACE token
        last_two_chars: str = self._get_last_two_chars_of_the_cur_token_text()
        match last_two_chars:
            case "\"{" | "'{" | "\\{":
                self._trim_last_char_add_pending_token_set_cur_token(self.LBRACE, "{", Token.DEFAULT_CHANNEL)

    def _get_last_two_chars_of_the_cur_token_text(self) -> str:
        text: str = self._cur_token.text
        return text[-2:] if len(text) >= 2 else text

    def _trim_last_char_add_pending_token_set_cur_token(self, type: int, text: str, channel: int) -> None:
        # trim the last char and add the modified curToken to the _pending_token_queue
        token_text_without_lbrace: str = self._cur_token.text[:-1]
        self._cur_token.text = token_text_without_lbrace
        self._cur_token.stop -= 1
        self.add_pending_token(self._cur_token)

        self._replace_current_token(type, text, channel)  # set _cur_token

    def _handle_COLONEQUAL_token_in_istring(self) -> None:  # istring = interpolated string (FSTRING or TSTRING)
        if self._lexer_mode_stack \
           and self._paren_or_bracket_opened_stack[-1] == 0:  # stack peek == 0

            # In an f/t-string, the walrus operator (:=) is only allowed inside parentheses.
            # If used outside, split the COLONEQUAL token into a COLON
            # (used as a format specifier instead of a walrus operator),
            # and move the equal sign to the beginning of the next token (FSTRING_MIDDLE or TSTRING_MIDDLE).
            self._cur_token.type = self.COLON
            self._cur_token.text = ":"
            self._cur_token.stop = self._cur_token.start

            match self._la_token.type:
                case self.FSTRING_MIDDLE | self.TSTRING_MIDDLE:
                    token: CommonToken = self._la_token.clone()
                    token.text = "=" + token.text
                    token.start -= 1
                    token.column -= 1
                    self._la_token = token
                case _:
                    self.add_pending_token(self._cur_token)
                    self._replace_current_token(self._active_interpolated_string_middle_token_type, "=", Token.DEFAULT_CHANNEL)
        self.add_pending_token(self._cur_token)

    def _replace_current_token(self, type: int, text: str, channel: int) -> None:
        token: CommonToken = self._cur_token.clone()
        token.type = type
        token.text = text
        token.channel = channel
        token.column += 1
        token.start += 1
        token.stop = token.start
        self._cur_token = token

    # ===================== Lexer‑Mode Management =====================
    # Manages the custom lexer mode stack used for interpolated strings.
    # Provides controlled push/pop operations independent of ANTLR’s
    # internal mode stack to ensure correct behavior in complex IString flows.

    def _push_lexer_mode(self, mode: int) -> None:
        self.pushMode(mode)
        self._lexer_mode_stack.append(self._cur_lexer_mode)  # stack push
        self._cur_lexer_mode = mode

    def _pop_lexer_mode(self) -> None:
        self.popMode()
        self._cur_lexer_mode = self._lexer_mode_stack.pop()

    # ===================== Format‑Specifier Handling =====================

    def _handle_FORMAT_SPECIFICATION_MODE(self) -> None:
        if not self._lexer_mode_stack or self._la_token.type != self.RBRACE:
            return

        # insert an empty FSTRING_MIDDLE or TSTRING_MIDDLE token instead of the missing format specification
        match self._cur_token.type:
            case self.COLON:
                self._create_and_add_pending_token(self._active_interpolated_string_middle_token_type, "", self._la_token)
            case self.RBRACE:
                # only when the previous brace expression is not a dictionary comprehension or set comprehension
                if not self._is_valid_dictionary_or_set_comprehension_expression(self._last_closed_brace_expression):
                    self._create_and_add_pending_token(self._active_interpolated_string_middle_token_type, "", self._la_token)

    # Determines whether the collected brace expression forms a dictionary or set
    # comprehension. Used to enforce Python’s rule that outermost f-string brace
    # expressions cannot be comprehensions.
    def _is_valid_dictionary_or_set_comprehension_expression(self, code: str) -> bool:
        from antlr4 import InputStream, CommonTokenStream
        from PythonLexer import PythonLexer
        from PythonParser import PythonParser

        input_stream: InputStream = InputStream(code)
        lexer: PythonLexer = PythonLexer(input_stream)
        token_stream: CommonTokenStream = CommonTokenStream(lexer)
        parser: PythonParser = PythonParser(token_stream)

        # Disable error listeners to suppress console output
        lexer.removeErrorListeners()
        parser.removeErrorListeners()

        parser.dictcomp()  # Try parsing as dictionary comprehension
        if parser.getNumberOfSyntaxErrors() == 0:
            return True

        parser = PythonParser(token_stream)
        token_stream.seek(0)
        parser.removeErrorListeners()
        parser.setcomp()  # Try parsing as set comprehension
        return parser.getNumberOfSyntaxErrors() == 0

    # ===================== Trailing‑Token Finalization =====================
    # Handles end-of-input cleanup, including emission of remaining DEDENT tokens,
    # final NEWLINE normalization, and generation of the EOF token to properly
    # terminate the logical token stream.

    def _insert_trailing_tokens(self) -> None:
        match self._last_pending_token_type_from_default_channel:
            case self.NEWLINE | self.DEDENT:
                pass  # no trailing NEWLINE token is needed
            case _:  # insert an extra trailing NEWLINE token that serves as the end of the last statement
                self._create_and_add_pending_token(self.NEWLINE, None, self._la_token)  # _la_token is EOF
        self._insert_INDENT_or_DEDENT_token(0)  # Now insert as much trailing DEDENT tokens as needed

    def _handle_EOF_token(self) -> None:
        if self._last_pending_token_type_from_default_channel > 0:
            # there was statement in the input (leading NEWLINE tokens are hidden)
            self._insert_trailing_tokens()
        self.add_pending_token(self._cur_token)

    # ===================== Pending‑Token Management =====================
    # Manages the queue of pending tokens, including emission of normal and hidden
    # tokens, preserving correct output order for the token stream.

    def _hide_and_add_pending_token(self, original_token: CommonToken) -> None:
        original_token.channel = Token.HIDDEN_CHANNEL
        self.add_pending_token(original_token)

    def _create_and_add_pending_token(self, ttype: int, text: Optional[str], original_token: CommonToken) -> None:
        token: CommonToken = original_token.clone()
        token.type = ttype
        token.channel = Token.DEFAULT_CHANNEL
        token.stop = original_token.start - 1
        token.text = ("<" + self.symbolicNames[ttype] + ">") if text is None else text

        self.add_pending_token(token)

    def add_pending_token(self, token: CommonToken) -> None:
        # save the last pending token type because the _pending_token_queue can be empty by the nextToken()
        self._previous_pending_token_type = token.type
        if token.channel == Token.DEFAULT_CHANNEL:
            self._last_pending_token_type_from_default_channel = self._previous_pending_token_type
        self._pending_token_queue.append(token)

    # ===================== Error Reporting & Diagnostics =====================
    # Provides consistent lexer-level error reporting, including generation of
    # ERRORTOKEN instances, construction of human-readable diagnostic messages,
    # and insertion of error markers into the token stream to ensure that the
    # parser receives accurate context for recovery.

    def _report_lexer_error(self, err_msg: str) -> None:
        self.getErrorListenerDispatch().syntaxError(self, self._cur_token.type, self._cur_token.line, self._cur_token.column, " LEXER" + PythonLexerBase.ERR_TXT + err_msg, None)

    def _report_error(self, err_msg: str) -> None:
        self._report_lexer_error(err_msg)

        self._create_and_add_pending_token(self.ERRORTOKEN, PythonLexerBase.ERR_TXT + err_msg, self._la_token)
        # the ERRORTOKEN also triggers a parser error
