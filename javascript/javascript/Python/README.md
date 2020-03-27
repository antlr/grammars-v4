# Python base Lexer and Parser for JavaScript
---
This is a base lexer and parser for the JavaScript language for Antlr4

Baecause the Lexer and Parser use special commands you first need to transform the grammar files:

- python transformGrammar.py JavaScriptParser.g4
- python transformGrammar.py JavaScriptLexer.g4

Then do the antlr thing:
- antlr4 -Dlanguage=Python3 -o gen *.g4

I have tested with the below code with a sampling of example JavaScript files from the examples directory.

```python
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