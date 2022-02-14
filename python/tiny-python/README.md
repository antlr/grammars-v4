# Tiny Python &nbsp; 
A considerably stripped down Python grammar for a starter Python (or a Python like) parser or even for educational purposes. 

The ANTLR4 parser grammar is based on the last "traditional" [Python grammar](https://docs.python.org/3.8/reference/grammar.html) which not yet written in [PEG](https://en.wikipedia.org/wiki/Parsing_expression_grammar).


### A simple usage example in command line:
```bash
antlr4 PythonLexer.g4
antlr4 PythonParser.g4
javac *.java
grun Python file_input -tokens test.py
```


### Related links:
[ANTLR 4](https://www.antlr.org/)

[ANTLR 4 Documentation](https://github.com/antlr/antlr4/tree/master/doc)

[ANTLR 4 Runtime API](https://www.antlr.org/api/Java/)

[Python 3 Lexical Analysis](https://docs.python.org/3/reference/lexical_analysis.html)

[ANTLR4 parser for Python 3.8.12](https://github.com/RobEin/ANTLR4-parser-for-Python-3.8.12)
