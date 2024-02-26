
from antlr4 import *

relativeImport = False
if __name__ is not None and "." in __name__:
    relativeImport = True

class CPP14ParserBase(Parser):
    @staticmethod
    def parser():
        if relativeImport:
            from .CPP14Parser import CPP14Parser
        else:
            from CPP14Parser import CPP14Parser
        return CPP14Parser

    def IsPureSpecifierAllowed(self) -> bool:
        try:
            x = self._ctx  # memberDeclarator
            c = x.getChild(0).getChild(0)
            c2 = c.getChild(0)
            p = c2.getChild(1)
            if p is None:
                return False
            yo = isinstance(p, self.parser().ParametersAndQualifiersContext)
            return yo
        except:
            pass
        return False
