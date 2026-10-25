# Using the Grammar

To correctly parse the language, you **must run the `RFilter` before using the main parser**.

## Why `RFilter` is required

The language uses **newline tokens (`NL`) whose meaning depends on the syntactic context**.
In some cases a newline should behave like a statement separator, while in other contexts it must be ignored.

`RFilter` is implemented as a secondary grammar (`RFilter.g4`) that processes the token stream produced by the lexer and adjusts the visibility of newline tokens. Specifically, it hides newline tokens that should not be treated as statement separators.

## Example

```python
from antlr4 import FileStream, CommonTokenStream
from RFilter import RFilter
from RLexer import RLexer
from RParser import RParser

# 1. Lex
lexer = RLexer(input_stream)
tokens = CommonTokenStream(lexer)

# 2. Run RFilter on the token stream
filter_parser = RFilter(tokens)
filter_parser.stream()   # start rule of the filter grammar

# 3. Reset the token stream
tokens.reset()

# 4. Parse with the main parser
parser = RParser(tokens)
parser.prog()
```