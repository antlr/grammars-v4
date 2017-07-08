# Alternative Python 3 parser

An ANTLR4 grammar for Python 3 based on version 3.3.5 of 
[The Python Language Reference](https://docs.python.org/3.3/reference/grammar.html).

Note that this grammar contains embedded Java code that handles
the insertion of `INDENT` and `DEDENT` tokens. The embedded code,
located inside the `NEWLINE` lexer rule as well as the `@lexer::members`
section is well documented, so people trying to port this grammar to
another target should not have much difficulty.

This parser builds on bkiers' 3.3.5 Python parser by adding the ability to have the
parser either throw informative-ish exceptions, or simply note and move on, when
encountering Python2-isms. In this manner, this parser will both serve to validate
and to detect Python2 code, for which there is no current ANTLR4 Java parser grammar.
