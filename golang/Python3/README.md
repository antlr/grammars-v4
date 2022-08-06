# Go parser for the Python3 target
---
This directory contains the base class code for the Go parser. To use the
Go grammar for Python3, run:
```
python transformGrammar.py
antlr4 -Dlanguage=Python3 -o gen *.g4
```
The [transformGrammar.py](https://github.com/antlr/grammars-v4/blob/master/golang/Python3/transformGrammar.py) script modifies the split grammar for Python3.
The grammar must be modified for Python3.

(Updated 20 July 2022 by Ken Domino.)
