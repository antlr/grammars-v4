from antlr4 import *

relativeImport = False
if __name__ is not None and "." in __name__:
    relativeImport = True

class bnfParserBase(Parser):
    @staticmethod
    def parser():
        if relativeImport:
            from .bnfParser import bnfParser
        else:
            from bnfParser import bnfParser
        return bnfParser

    def NotNL(self):
        bnfParser = self.parser()
        i = 1
        c = self._input.LT(i)
        return c.type != bnfParser.NL

    def not_assign(self):
        i = 1
        c = self._input.LT(i)
        while c is not None and c.type == bnfLexer.WS:
            i += 1
            c = self._input.LT(i)
        return not (c.type == bnfLexer.ASSIGN1 or
                    c.type == bnfLexer.ASSIGN2 or
                    c.type == bnfLexer.ASSIGN3 or
                    c.type == bnfLexer.ASSIGN4)

    def not_lhs(self):
        i = 1
        c = self._input.LT(i)
        while c is not None and c.type == bnfLexer.WS:
            i += 1
            c = self._input.LT(i)
        if c is not None and c.type != bnfLexer.X1:
            return True
        # '<'
        while True:
            while c is not None and c.type == bnfLexer.WS:
                i += 1
                c = self._input.LT(i)
            if c is None or c.type not in (bnfLexer.ID, bnfLexer.X2):
                return True
            # ID
            if c.type == bnfLexer.X2:
                break
        # '>'
        while c is not None and c.type == bnfLexer.WS:
            i += 1
            c = self._input.LT(i)
        return not (c.type in (bnfLexer.ASSIGN1, bnfLexer.ASSIGN2, bnfLexer.ASSIGN3, bnfLexer.ASSIGN4))

        