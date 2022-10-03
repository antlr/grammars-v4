import sys
from enum import Enum
from pkgutil import get_data
from typing import List, Optional, Dict, TextIO
from antlr4 import Lexer, Token, InputStream, Parser, TokenStream
from antlr4.Token import CommonToken

class PythonLexerBase(Lexer):
    tab_size = 4

    def __init__(self, input_stream: InputStream, output: TextIO = sys.stdout):
        super().__init__(input_stream, output)

        self.__opened: int = 0
        self.__indents: List[int] = []
        self.__first_tokens_ind: int = 0
        self.__last_tokens_ind: int = 0
        self.__buffer: List[Optional[Token]] = [None for _ in range(32)]
        self.__last_token: Optional[Token] = None

    def emitToken(self, token: Token) -> None:
        self._token = token

        if self.__buffer[self.__first_tokens_ind] is not None:
            self.__last_tokens_ind = self.__inc_token_ind(self.__last_tokens_ind)

            if self.__last_tokens_ind == self.__first_tokens_ind:
                # Enlarge buffer
                new_array: List[Optional[Token]] = [None for _ in range(len(self.__buffer) * 2)]
                dest_ind = len(new_array) - (len(self.__buffer) - self.__first_tokens_ind)

                new_array[0:self.__first_tokens_ind] = self.__buffer[0:self.__first_tokens_ind]
                new_array[dest_ind:dest_ind + len(self.__buffer) - self.__first_tokens_ind] = \
                    self.__buffer[self.__first_tokens_ind:len(self.__buffer)]

                self.__first_tokens_ind = dest_ind
                self.__buffer = new_array

        self.__buffer[self.__last_tokens_ind] = token
        self.__last_token = token

    def nextToken(self) -> Token:
        # Check if the end-of-file is ahead and there are still some DEDENTS expected.
        if self._input.LA(1) == Token.EOF and self.__indents:
            if (self.__buffer[self.__last_tokens_ind] is not None or
                self.__buffer[self.__last_tokens_ind].type != self.LINE_BREAK):
                # First emit an extra line break that serves as the end of the statement.
                self.__emit_token_type(self.LINE_BREAK)

            # Now emit as much DEDENT tokens as needed.
            while self.__indents:
                self.__emit_token_type(self.DEDENT)
                self.__indents.pop()

        next_token: Token = super().nextToken()

        if self.__buffer[self.__first_tokens_ind] is None:
            return next_token

        result: Token = self.__buffer[self.__first_tokens_ind]
        self.__buffer[self.__first_tokens_ind] = None

        if self.__first_tokens_ind != self.__last_tokens_ind:
            self.__first_tokens_ind = self.__inc_token_ind(self.__first_tokens_ind)

        return result

    def HandleNewLine(self) -> None:
        self.__emit_token_type_on_channel(self.NEWLINE, self.HIDDEN, self.text)

        c = self._input.LA(1)
        if c == -1 :
            return
        next_char: str = chr(c)

        # Process whitespaces in handle_spaces
        if next_char != ' ' and next_char != '\t' and self.__is_not_new_line_or_comment(next_char):
            self.__process_new_line(0)

    def HandleSpaces(self) -> None:
        next_char: str = chr(self._input.LA(1))

        if ((self.__last_token is None or self.__last_token.type == self.NEWLINE) and
                self.__is_not_new_line_or_comment(next_char)):
            # Calculates the indentation of the provided spaces, taking the
            # following rules into account:
            #
            # "Tabs are replaced (from left to right) by one to eight spaces
            #  such that the total number of characters up to and including
            #  the replacement is a multiple of eight [...]"
            #
            #  -- https://docs.python.org/3.1/reference/lexical_analysis.html#indentation

            indent: int = 0

            for i in range(0, len(self.text)):
                indent += PythonLexerBase.tab_size - indent % PythonLexerBase.tab_size if self.text[i] == '\t' else 1

            self.__process_new_line(indent)

        self.__emit_token_type_on_channel(self.WS, self.HIDDEN, self.text)

    def IncIndentLevel(self) -> None:
        self.__opened += 1

    def DecIndentLevel(self) -> None:
        if self.__opened:
            self.__opened -= 1

    def __is_not_new_line_or_comment(self, next_char: str) -> bool:
        return (self.__opened == 0 and
                next_char != '\r' and
                next_char != '\n' and
                next_char != '\f' and
                next_char != '#')

    def __process_new_line(self, indent: int) -> None:
        self.__emit_token_type(self.LINE_BREAK)

        previous: int = 0 if not self.__indents else self.__indents[-1]

        if indent > previous:
            self.__indents.append(indent)
            self.__emit_token_type(self.INDENT)
        else:
            # Possibly emit more than 1 DEDENT token.
            while self.__indents and self.__indents[-1] > indent:
                self.__emit_token_type(self.DEDENT)
                self.__indents.pop()

    def __inc_token_ind(self, ind: int) -> int:
        return (ind + 1) % len(self.__buffer)

    def __emit_token_type(self, token_type: int) -> None:
        self.__emit_token_type_on_channel(token_type, self.DEFAULT_TOKEN_CHANNEL, "")

    def __emit_token_type_on_channel(self, token_type: int, channel: int, text: str) -> None:
        char_index: int = self.getCharIndex()
        token: CommonToken = CommonToken(
            self._tokenFactorySourcePair,
            token_type,
            channel,
            char_index - len(text),
            char_index - 1)
        token.line = self.line
        token.column = self.column
        token.text = text

        self.emitToken(token)


class PythonVersion(Enum):
    Autodetect = 0
    Python2 = 2
    Python3 = 3


class PythonParserBase(Parser):
    def __init__(self, input_stream: TokenStream):
        super().__init__(input_stream)
        self.__version = PythonVersion.Autodetect

    @property
    def version(self) -> PythonVersion:
        return self.__version

    @version.setter
    def version(self, version: PythonVersion | int):
        if isinstance(version, PythonVersion):
            self.__version = version
        else:
            self.__version = PythonVersion(version)

    def _check_version(self, version: int) -> bool:
        return self.__version == PythonVersion.Autodetect or version == self.__version.value

    def set_version(self, required_version: int) -> None:
        self.__version = PythonVersion(required_version)
