# Rexx Grammar

An ANTLR4 grammar for Rexx based on the ANSI X3.274-1996 standard, and informed
by creator Mike Cowlishaw's "The Rexx Language", 2nd edition (aka TRL2).

Initial verssion by Aleksandr Shcherbakov (canduduslynx@gmail.com), based on the
ANSI standard.

## How to use

* Generate lexer and parser with ANTLR4 generator
* Include both generated and base classes to project from corresponding
  directory (_e.g._, Java or Python)

## License

[Apache 2.0](http://www.apache.org/licenses/LICENSE-2.0)

# Targets

## Java

## Python3

To use the Rexx grammar for Python3, run:

```bash
cd Python3
cp ..\*.g4 .
antlr4 -Dlanguage=Python3 *.g4
```
