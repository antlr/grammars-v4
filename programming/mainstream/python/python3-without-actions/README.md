# Python 3 parser without embedded grammar actions &nbsp; [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

A Python 3 parser that based on the Bart Kiers's Python 3.3 grammar with an improved indentation handling outside of the grammar with the following advantages:
-  more informative token metadata
-  reusable code for grammar with embedded action and without action
-  detection of various indentation errors

#### Some indentation errors with message:
```python
 i = 1 # first line begins with space
#  line 1:1	IndentationError: unexpected indent


if i == 1:
j = 0
#  line 2:0	IndentationError: expected an indented block


if i == 1:
    j = 0
        k = 0
#  line 3:8	IndentationError: unexpected indent


if i == 1:
    j = 0
  k = 0
#  line 3:2	IndentationError: unindent does not match any outer indentation level
```

## How to use
```bash
antlr4 Python3.g4
javac *.java
java Main test.py
```

## Related links
[The Python 3.3.7 Language Reference](https://docs.python.org/3.3/reference/grammar.html)

[Bart Kiers's Python 3.3 ANTLR4 grammar](https://github.com/bkiers/python3-parser)

[Tiny Python](https://github.com/antlr/grammars-v4/tree/master/python/tiny-python)


