# Python 3.14.2 parser

### About files:
- PythonParser.g4 is the ANTLR4 parser grammar that based on the official [Python PEG grammar](https://docs.python.org/3.14/reference/grammar.html)

- PythonLexerBase class
    - handles the Python indentations
    - creates encoding token
    - tokenizes fstring literals
    - and manage many other things

- Example files from: [Python 3.14 Standard Lib](https://github.com/python/cpython/tree/3.14/Lib)<br/><br/>

### Recent changes:
- parser grammar update for Python 3.14.2
- tokenizing t-string literals
- tokenizing BOM Unicode character at the start of the file so it is skipped in the token stream
- moved encoding detection from PythonLexerBase to a separate component

#### [Previous changes](https://github.com/antlr/grammars-v4/blob/master/python/python3_14/changes.md)<br/><br/>

### Related link:
[ANTLR4-parser-for-Python-3.14](https://github.com/RobEin/ANTLR4-parser-for-Python-3.14)