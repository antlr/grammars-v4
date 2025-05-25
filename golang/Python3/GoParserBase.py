import sys
from antlr4 import *
if sys.version_info[1] > 5:
    from typing import TextIO
else:
    from typing.io import TextIO

class GoParserBase(Parser):

    debug = False

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.table = set()

    def myreset(self):
        la = 1
    
    def closingBracket(self) -> bool:
        la = self._input.LT(1)
        return la.type == self.R_PAREN or la.type == self.R_CURLY or la.type == Token.EOF

    def isNotReceive(self) -> bool:
        la = self._input.LT(2)
        return la.type != self.RECEIVE

    def addImportSpec(self):
        ctx = self._ctx
        if not isinstance(ctx, self.ImportSpecContext):
            return
        importSpec = ctx
        if importSpec.packageName() is not None:
            name = importSpec.packageName().getText()
            if self.debug:
                print("Entering", name)
            self.table.add(name)
        else:
            name = importSpec.importPath().getText()
            name = name.replace("\"", "")
            name = name.replace("\\", "/")
            path_arr = name.split('/')
            file_arr = path_arr[-1].split('.')
            file_name = file_arr[-1]
            if self.debug:
                print("Entering", file_name)
            self.table.add(file_name)

    def isOperand(self) -> bool:
        la = self._input.LT(1)
        if la.text == "err":
            return True
        result = True
        if la.type != self.IDENTIFIER:
            if self.debug:
                print(f"isOperand Returning {result} for {la.text}")
            return result
        result = (la.text in self.table)
        la2 = self._input.LT(2)
        if la2.type != self.DOT:
            result = True
            if self.debug:
                print(f"isOperand Returning {result} for {la.text}")
            return result
        la3 = self._input.LT(3)
        if la3.type == self.L_PAREN:
            result = True
            if self.debug:
                print(f"isOperand Returning {result} for {la.text}")
            return result
        if self.debug:
            print(f"isOperand Returning {result} for {la.text}")
        return result

    def isConversion(self) -> bool:
        la = self._input.LT(1)
        result = (la.type != self.IDENTIFIER)
        if self.debug:
            print(f"isConversion Returning {result} for {la.text}")
        return result

    def isMethodExpr(self) -> bool:
        la = self._input.LT(1)
        result = True
        if la.type == self.STAR:
            if self.debug:
                print(f"isMethodExpr Returning {result} for {la.text}")
            return result
        if la.type != self.IDENTIFIER:
            result = False
            if self.debug:
                print(f"isMethodExpr Returning {result} for {la.text}")
            return result
        result = (la.text not in self.table)
        if self.debug:
            print(f"isMethodExpr Returning {result} for {la.text}")
        return result
