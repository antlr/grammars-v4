from antlr4 import *

class Python3ParserBase(Parser):

    def CannotBePlusMinus(self) -> bool:
        return True

    def CannotBeDotLpEq(self) -> bool:
        return True
