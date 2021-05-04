# Java9 Grammar

## Source
The source for the grammar is the
[Java Langauage Specification, version 9](https://docs.oracle.com/javase/specs/jls/se9/html/index.html).
Please note that this grammar is unoptimized: the runtimes for parses will
be significantly slower than the [optimized Java grammar](https://github.com/antlr/grammars-v4/tree/master/java/java).

NB: This grammar uses the [Antlr4 tool and runtime](https://www.antlr.org/download.html)
for full Unicode code points and general categories.
Harwell's Antlr4cs alternative tool
and runtime for Antlr4 is an older version of Antlr4 that
does not support the Unicode feature. An older version of the grammar
is provided for back support.

## Examples
Several of these examples come from the OpenJDK and
Android runtime source code.

May 3, 2021
