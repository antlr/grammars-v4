# Target-specific grammar instructions.
---
This directory contains the base class code for a target-specific parser. To use, run:
```
python transformGrammar.py
antlr4 -Dlanguage=Python3 -o gen *.g4
```
The transformGrammar.py script modifies the grammar for the target.

(Updated 20 July 2022 by Ken Domino.)
