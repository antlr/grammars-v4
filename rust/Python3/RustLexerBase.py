from typing import TextIO, Optional
from antlr4 import *
from antlr4.InputStream import InputStream

class RustLexerBase(Lexer):
    flt_mp = set()
    RustLexer = None
    def __init__(self, input: InputStream, output: TextIO = ...) -> None:
        super().__init__(input, output)
        self.token_lookbehind: tuple[Optional[Token], Optional[Token]] = (None, None)
        try:
            from .RustLexer import RustLexer
        except ImportError:
            from RustLexer import RustLexer
        RustLexerBase.RustLexer = RustLexer
        RustLexerBase.flt_mp = {
                RustLexer.STRING_LITERAL,
                RustLexer.RAW_STRING_LITERAL,
                RustLexer.BYTE_LITERAL,
                RustLexer.BYTE_STRING_LITERAL,
                RustLexer.RAW_BYTE_STRING_LITERAL,
                RustLexer.INTEGER_LITERAL,
                RustLexer.DEC_LITERAL,
                RustLexer.HEX_LITERAL,
                RustLexer.OCT_LITERAL,
                RustLexer.BIN_LITERAL,
                RustLexer.KW_SUPER,
                RustLexer.KW_SELFVALUE,
                RustLexer.KW_SELFTYPE,
                RustLexer.KW_CRATE,
                RustLexer.KW_DOLLARCRATE,
                RustLexer.RCURLYBRACE,
                RustLexer.RSQUAREBRACKET,
                RustLexer.RPAREN,
                RustLexer.KW_AWAIT,
                RustLexer.NON_KEYWORD_IDENTIFIER,
                RustLexer.RAW_IDENTIFIER,
                RustLexer.KW_MACRORULES,   
                RustLexer.GT 
            }

        """LOOK BEHIND TOKENS"""
    def nextToken(self):
        next: Token = super().nextToken()

        if next.channel == Token.DEFAULT_CHANNEL:
            self.token_lookbehind = self.token_lookbehind[1], next

        return next

    def SOF(self):
        return self._input.LA(-1) <= 0

    def next(self, expect) -> bool:
        if isinstance(expect, str):
            return chr(self._input.LA(1)) == expect
        else:
            return self._input.LA(1) == expect

     def nexti(self, expect) -> bool:
        if isinstance(expect, str):
            return chr(self._input.LA(1)) == expect
        else:
            return self._input.LA(1) == expect

    def floatDotPossible(self):
        next = chr(self._input.LA(1))
        # print(f'INFO: floatpossible ? {next} = {chr(next)}')
        # if isinstance(next, int):

        #     next = RustLexer.literalNames[next]
        #     next = next[1:-1]
        # if not isinstance(next, str):
        #     next = next.text

        if next in [".", "_"]:
            return False
        if next == "f":
            if chr(self._input.LA(2)) == "3" and chr(self._input.LA(3)) == "2":
                return True
            if chr(self._input.LA(2)) == "6" and self._input.LA(3) == "4":
                return True
            return False
        if next.isalpha():
            return False
        return True

    def floatLiteralPossible(self):
        prev, current = self.token_lookbehind
        
        if prev == None or current == None:
            return True
        elif current.type != RustLexerBase.RustLexer.DOT:
            return True
        else:
            return prev.type not in RustLexerBase.flt_mp 
