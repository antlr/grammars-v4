import sys
from antlr4 import Parser

ALL_SEMANTIC_FUNCTIONS = ["IsLocalVariableDeclaration"]


def parseNoSemantics(args):
    result = set()
    for a in args:
        if a.lower().startswith("--no-semantics"):
            eq = a.find('=')
            if eq == -1:
                for f in ALL_SEMANTIC_FUNCTIONS:
                    result.add(f)
            else:
                for f in a[eq + 1:].split(','):
                    result.add(f.strip())
    return result


class CSharpParserBase(Parser):

    def __init__(self, input, output=sys.stdout):
        super().__init__(input, output)
        self._noSemantics = parseNoSemantics(sys.argv)

    def IsLocalVariableDeclaration(self):
        if "IsLocalVariableDeclaration" in self._noSemantics:
            return True
        if "." in __name__:
            from .CSharpParser import CSharpParser
        else:
            from CSharpParser import CSharpParser
        local_var_decl = self._ctx
        if not isinstance(local_var_decl, CSharpParser.Local_variable_declarationContext):
            return True
        local_variable_type = local_var_decl.local_variable_type()
        if local_variable_type is None:
            return True
        if local_variable_type.getText() == "var":
            return False
        return True
