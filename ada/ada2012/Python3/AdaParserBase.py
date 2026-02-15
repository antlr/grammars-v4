from antlr4 import *
from antlr4.Token import CommonToken
import sys
from typing import TextIO


class AdaParserBase(Parser):

    def __init__(self, input: TokenStream, output: TextIO = sys.stdout):
        super().__init__(input, output)

    def ParsePragmas(self):
        if "." in __name__:
            from .AdaLexer import AdaLexer
            from .AdaParser import AdaParser
        else:
            from AdaLexer import AdaLexer
            from AdaParser import AdaParser
        stream = self._input
        stream.fill()
        all_tokens = stream.tokens
        PRAGMA_CHANNEL = 2
        current_pragma = None
        pragmas = []
        for token in all_tokens:
            if token.channel != PRAGMA_CHANNEL:
                continue
            if token.type == AdaLexer.PRAGMA:
                current_pragma = [token]
            elif current_pragma is not None:
                current_pragma.append(token)
                if token.type == AdaLexer.SEMI:
                    pragmas.append(current_pragma)
                    current_pragma = None
        for pragma_tokens in pragmas:
            default_channel_tokens = []
            for t in pragma_tokens:
                ct = CommonToken(source=(t.source[0], t.source[1]), type=t.type)
                ct.text = t.text
                ct.line = t.line
                ct.column = t.column
                ct.start = t.start
                ct.stop = t.stop
                ct.tokenIndex = t.tokenIndex
                ct.channel = Token.DEFAULT_CHANNEL
                default_channel_tokens.append(ct)
            eof = CommonToken(type=Token.EOF)
            eof.channel = Token.DEFAULT_CHANNEL
            default_channel_tokens.append(eof)
            token_source = CommonTokenStream(InputStream(""))
            token_source.tokens = default_channel_tokens
            token_source.index = 0
            token_source.fetchedEOF = True
            parser = AdaParser(token_source)
            parser.removeErrorListeners()
            for listener in self._listeners:
                parser.addErrorListener(listener)
            parser.pragmaRule()
