### Python 3 target

#### Command line example:
- first of all copy the two grammar files and the test.py to this directory

Unix:
```bash
    cp ../*.g4 .
    cp ../test.py .
```

Windows:
```bash
    copy ..\*.g4
    copy ..\test.py
```

```bash
antlr4 -Dlanguage=Python3 PythonLexer.g4
antlr4 -Dlanguage=Python3 PythonParser.g4
pygrun --tokens Python file_input test.py
pygrun --tree Python file_input test.py
```

#### Related link:
[Python target](https://github.com/antlr/antlr4/blob/master/doc/python-target.md)
