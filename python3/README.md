# Python 3 parser

An ANTLR4 grammar for Python 3 based on version 3.3.5 of 
[The Python Language Reference](https://docs.python.org/3.3/reference/grammar.html).

For examples and unit tests, which consist of parsing all Python 
source files from Python 3's 
[standard library](http://hg.python.org/cpython/file/default/Lib/), 
see [this GitHub repository](https://github.com/bkiers/python3-parser).

Note that this grammar contains embedded Java code that handles
the insertion of `INDENT` and `DEDENT` tokens. The embedded code,
located inside the `NEWLINE` lexer rule as well as the `@lexer::members`
section is well documented, so people trying to port this grammar to
another target should not have much difficulty.
