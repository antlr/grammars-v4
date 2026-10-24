from antlr4 import *

relativeImport = False
if __name__ is not None and "." in __name__:
    relativeImport = True

class PostgreSQLParserBase(Parser):
    def ParseRoutineBody(self):
        return

    def OnlyAcceptableOps(self):
        c = self._input.LT(1)
        text = c.text
        return text == "!" or text == "!!" or text == "!=-"
