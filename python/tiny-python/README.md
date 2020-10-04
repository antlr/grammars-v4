# Tiny Python &nbsp; [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A considerably stripped down Python grammar for a starter Python (or Python like) parser or even for educational purposes. 

The ANTLR4 grammars are based on the Bart Kiers's Python 3.3 grammar with an improved indentation handling with the following advantages:
-  more informative token metadata
-  reusable code for grammar with embedded action and without action
-  detection of various indentation errors


## How to use
#### grammar with actions:
```bash
antlr4 Python3.g4
javac *.java
grun Python3 file_input -tokens test.py
```

#### grammar without actions:
```bash
antlr4 Python3.g4
javac *.java
java Main test.py
```

## Related links
[ANTLR 4](https://www.antlr.org/)

[ANTLR 4 Documentation](https://github.com/antlr/antlr4/blob/4.7.2/doc/index.md)

[ANTLR 4 Runtime API](https://www.antlr.org/api/Java/)

[The Python 3.3.7 Language Reference](https://docs.python.org/3.3/reference/grammar.html)

[Bart Kiers's Python 3.3 ANTLR4 grammar](https://github.com/bkiers/python3-parser)

[Python 3 parser without embedded grammar actions](https://github.com/antlr/grammars-v4/tree/master/python/python3-without-actions)


