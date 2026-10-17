import sys
from antlr4 import *
if sys.version_info[1] > 5:
    from typing import TextIO
else:
    from typing.io import TextIO

class GoParserBase(Parser):

    debug = False

    @staticmethod
    def _has_arg(args, arg):
        arg_lower = arg.lower()
        for a in args:
            if arg_lower in a.lower():
                return True
        return False

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.table = set()
        GoParserBase.debug = GoParserBase._has_arg(sys.argv, '--debug')
        if GoParserBase.debug:
            print("debug =", GoParserBase.debug)

    def myreset(self):
        self.table = set()
    
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
            return
        import_path = importSpec.importPath()
        if import_path is None:
            return
        name = import_path.getText()
        if self.debug:
            print("import path", name)
        name = name.replace("\"", "")
        if len(name) == 0:
            return
        name = name.replace("\\", "/")
        path_arr = name.split('/')
        if len(path_arr) == 0:
            return
        last_component = path_arr[-1]
        if len(last_component) == 0:
            return
        # Handle special cases like "." and ".."
        if last_component == "." or last_component == "..":
            return
        file_arr = last_component.split('.')
        # Guard against empty array (can happen if last_component is all dots)
        if len(file_arr) == 0:
            self.table.add(last_component)
            if self.debug:
                print("Entering", last_component)
            return
        file_name = file_arr[-1]
        if len(file_name) == 0:
            # Fall back to last_component if split resulted in empty string
            file_name = last_component
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

    # Built-in functions that take a type as first argument
    BUILTIN_TYPE_FUNCTIONS = {"make", "new"}

    # Check if we're in a call to a built-in function that takes a type as first argument.
    # Called after L_PAREN has been matched in the arguments rule.
    def isTypeArgument(self) -> bool:
        # After matching L_PAREN, LT(-1) is '(' and LT(-2) is the token before it
        func_token = self._input.LT(-2)
        if func_token is None or func_token.type != self.IDENTIFIER:
            if self.debug:
                print("isTypeArgument Returning False - no identifier before (")
            return False
        result = func_token.text in self.BUILTIN_TYPE_FUNCTIONS
        if self.debug:
            print(f"isTypeArgument Returning {result} for {func_token.text}")
        return result

    # Check if we're NOT in a call to a built-in function that takes a type.
    # This is the inverse of isTypeArgument for the expressionList alternative.
    def isExpressionArgument(self) -> bool:
        result = not self.isTypeArgument()
        if self.debug:
            print(f"isExpressionArgument Returning {result}")
        return result
