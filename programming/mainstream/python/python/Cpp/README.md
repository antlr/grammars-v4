# Target-specific grammar instructions.
---
This directory contains the base class code for a target-specific parser. To use, run:
```
python transformGrammar.py
antlr4 -Dlanguage=Cpp -o gen *.g4
```
The transformGrammar.py script modifies the grammar for the target.

(Updated 3 Aug 2022 by Ken Domino.)
