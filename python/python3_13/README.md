# Python 3.13.2 parser

### About files:
- PythonParser.g4 is the ANTLR4 parser grammar that based on the official [Python PEG grammar](https://docs.python.org/3.13/reference/grammar.html)

- PythonLexerBase class
    - handles the Python indentations
    - creates encoding token
    - tokenizes fstring literals
    - and manage many other things

- Example files from: [Python 3.13 Standard Lib](https://github.com/python/cpython/tree/3.13/Lib)<br/><br/>

### Recent changes:
- parser grammar update for Python 3.13.2
- added ENCODING token
- complete rewrite of fstring tokenizer in lexer grammar and PythonLexerBase class
    - now correctly tokenizes the followings in fstring:
        - escape sequences
        - walrus operator
        - dictionary comprehension
        - set comprehension
- soft keywords changes:
    - no embedded code (semantic predicates) in parser grammar for soft keywords
    - no need for PythonParserBase class
    - no need for transformGrammar.py
    - **BREAKING CHANGES**:
        - dedicated tokens for soft keywords instead of NAME token:
            - NAME_OR_TYPE
            - NAME_OR_MATCH
            - NAME_OR_CASE
            - NAME_OR_WILDCARD

#### [Previous changes](https://github.com/antlr/grammars-v4/tree/master/python/python3_13)<br/><br/>

### Related link:
[ANTLR4-parser-for-Python-3.13](https://github.com/RobEin/ANTLR4-parser-for-Python-3.13)