import sys
import re
from antlr4 import Parser
from antlr4.error.ErrorListener import ErrorListener as BaseErrorListener

class ErrorListener(BaseErrorListener):
    def __init__(self, quiet, tee, out):
        super().__init__()
        self.had_error = False
        self._quiet = quiet
        self._tee = tee
        self._out = out

    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        fileName = "<unknown>"
        lineAdjusted = line

        if isinstance(recognizer, Parser):
            from CLexer import CLexer
            ts2 = recognizer._input
            ind = offendingSymbol.tokenIndex
            j = ind
            while j >= 0:
                t = ts2.get(j)
                if t is None:
                    break
                if t.type == CLexer.LineDirective:
                    txt = t.text
                    parts = re.split(r'\s+', txt)
                    if len(parts) >= 3:
                        try:
                            dirLine = int(parts[1])
                            lineDirective = t.line
                            lineDiff = line - lineDirective
                            lineAdjusted = lineDiff + dirLine - 1
                            fileName = parts[2].strip()
                        except ValueError:
                            pass
                    break
                j -= 1

        self.had_error = True
        if self._tee and self._out is not None:
            self._out.write(fileName + " line " + str(lineAdjusted) + ", .p " + str(line) + ":" + str(column) + " " + msg + "\n")
        if not self._quiet:
            print(fileName + " line " + str(lineAdjusted) + ", .p " + str(line) + ":" + str(column) + " " + msg, file=sys.stderr)
