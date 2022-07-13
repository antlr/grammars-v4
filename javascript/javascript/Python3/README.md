# JavaScript parser for the Python3 target
---
This directory contains the base class code for the JavaScript parser. To use the
JavaScript grammar for Python3, run:
```
python transformGrammar.py
antlr4 -Dlanguage=Python3 -o gen *.g4
```
The [transformGrammar.py](https://github.com/antlr/grammars-v4/blob/master/javascript/javascript/Python3/transformGrammar.py) script modifies the split grammar for Python3.
The grammar must be modified for Python3.

The generate parser works with this Python3 driver:

```
import sys
from antlr4 import *
import JavaScriptLexer
import JavaScriptParser

JSL = JavaScriptLexer.JavaScriptLexer
JSP = JavaScriptParser.JavaScriptParser

class WriteTreeListener(ParseTreeListener):
    def visitTerminal(self, node:TerminalNode):
        print ("Visit Terminal: " + str(node) + " - " + repr(node))

def main(argv):
    input_stream = FileStream(argv[1])
    print("Test started for: " + argv[1])
    lexer = JSL(input_stream)
    stream = CommonTokenStream(lexer)
    parser = JSP(stream)
    print("Created parsers")
    tree = parser.program()
    ParseTreeWalker.DEFAULT.walk(WriteTreeListener(), tree)

if __name__ == '__main__':
    print("Running")
    main(sys.argv)
```

(Updated 13 July 2022 by Ken Domino.)
