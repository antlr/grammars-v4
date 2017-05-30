# Python 3 parser

An ANTLR4 grammar for Python 3 based on version 3.6 of 
[The Python Language Reference](https://docs.python.org/3/reference/grammar.html).

This grammar has been tested against the Python 3's 
[standard library](https://hg.python.org/cpython/file/3.6/Lib), 

Note that this grammar contains embedded Java code that handles
the insertion of `INDENT` and `DEDENT` tokens. The embedded code,
located inside the `NEWLINE` lexer rule as well as the `@lexer::members`
section is well documented, so people trying to port this grammar to
another target should not have much difficulty.
