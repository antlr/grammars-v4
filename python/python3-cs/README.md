# Python 3 parser

An ANTLR4 grammar for Python 3 based on version 3.6 of 
[The Python Language Reference](https://docs.python.org/3/reference/grammar.html).

This grammar has been tested against the Python 3's 
[standard library](https://hg.python.org/cpython/file/3.6/Lib), 
the contents of the asyncio folder there is included in the examples section.

Also it has beed tested against the `test_grammar.py` file from the
Python3 target.

Note that there are multiple grammars here: Java, Python3, JS, TS and C#
targets. Target of grammars are mentioned at the top of the grammar 
(this one is C# target).
This grammar contains embedded code that handles
the insertion of `INDENT` and `DEDENT` tokens. The embedded code,
located inside the `NEWLINE` lexer rule as well as the `@lexer::members`
section is well documented, so people trying to port this grammar to
another target should not have much difficulty.

NOTICE: this grammar works only with 
[Sam Harwell's runtime library](https://github.com/sharwell/antlr4cs) 
due to a private field `Lexer._tokenFactorySourcePair` in 
the official runtime package