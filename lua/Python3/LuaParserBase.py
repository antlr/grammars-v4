import sys
from antlr4 import *
if sys.version_info[1] > 5:
    from typing import TextIO
else:
    from typing.io import TextIO

class LuaParserBase(Parser):

    debug = False

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
    
    def IsFunctionCall(self) -> bool:
        la = self._input.LT(1)
        if la.type != self.NAME:
            return False
        la = self._input.LT(2)
        if la.type == self.OP:
            return False
        return True

