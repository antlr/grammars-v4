
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
            x = self.Context  # memberDeclarator
            c = x.get_child(0).get_child(0)
            c2 = c.get_child(0)
            p = c2.get_child(1)
            if p is None:
                return False
            return isinstance(p, CPP14Parser.ParametersAndQualifiersContext)
        except:
            pass
        return False

