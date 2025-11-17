from antlr4 import *
try:
    from .RustLexer import RustLexer
except ImportError:
    from RustLexer import RustLexer


class RustParserBase(Parser):

    def NextGT(self) -> bool:
        return self._input.LA(1) == RustLexer.literalNames.index(f"'>'")

    def NextLT(self) -> bool:
        return self._input.LA(1) == RustLexer.literalNames.index(f"'<'")
