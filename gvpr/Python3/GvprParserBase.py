from antlr4 import *
from gvprLexer import gvprLexer;

class GvprParserBase(Parser):

    def IsSemiRequired(self) -> bool:
        c = self._input.LT(-1)
        d = self._input.LT(1)
        return c.type != gvprLexer.CCBC

    def IsSemiNotRequired(self) -> bool:
        c = self._input.LT(-1)
        d = self._input.LT(1)
        return c.type == gvprLexer.CCBC
        