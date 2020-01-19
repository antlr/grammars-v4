# Python 3 grammar without actions &nbsp; [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

The [ANTLR4](https://www.antlr.org/) grammar is based on the [Bart Kiers's Python 3.3 grammar](https://github.com/bkiers/python3-parser) with an improved indent/dedent handling with the following advantages:
-  warning for mixture of space and tab indentation
-  advanced token metadata information (see grun)
-  reusable code for grammar with actions and without actions
-  detection of [inconsistent dedent](https://docs.python.org/2.5/ref/indentation.html) (half dedent):
```python
    # for example
    if i == 1:
            j = 1
        k = 1
```

## How to use
```bash
antlr4 Python3.g4
javac *.java
java Main test.py
```

## Related links

[ANTLR 4 Documentation](https://github.com/antlr/antlr4/blob/4.7.2/doc/index.md)

[The Python 3.3.7 Language Reference](https://docs.python.org/3.3/reference/grammar.html)

[Bart Kiers's Python 3.3 ANTLR4 grammar](https://github.com/bkiers/python3-parser)

[Python starter (tiny Python)](https://github.com/RobEin/python3-parser)


