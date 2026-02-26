# Kumir Grammar for ANTLR v4

This directory contains ANTLR v4 grammars (Lexer and Parser) for the Kumir (КуМир) programming language. Kumir is a
Russian algorithmic language primarily used for teaching programming in schools.

## Language Features Covered

* Core language keywords (алг, нач, кон, если, то, иначе, все, нц, кц, для, пока, etc.)
* Basic data types (цел, вещ, лог, сим, лит) and table types (целтаб, etc.)
* Common actor types (файл, цвет - others can be added if needed)
* Variable declarations (including initialization with `=`)
* Assignment (`:=`) including function result assignment (`знач := ...`)
* Expressions (arithmetic, logical, comparison) with standard operator precedence
* Control flow structures (if-then-else, switch-case, various loops: for, while, N times, simple loop)
* Procedure and function definitions (including parameters: арг, рез, аргрез)
* Algorithm names: Supports multi-word names, including those containing numbers or keywords (like `И`), as observed in
  the reference Kumir IDE. This is handled by the `algorithmNameTokens` rule in the parser.
* Basic module structure (`использовать`, `модуль`, `конец модуля`)
* Comments (`|` and `#`)
* Literals (integers, reals, strings, chars, booleans `да`/`нет`, color constants)
* Array literals (`{...}`)

## Grammar Files

* `KumirLexer.g4`: Defines the tokens for the Kumir language. Uses UTF-8 encoding. Handles case-insensitivity for
  Cyrillic keywords. It is recommended to use UTF-8 encoding **without BOM** for Kumir source files.
* `KumirParser.g4`: Defines the parsing rules based on the tokens from the lexer. Uses `tokenVocab` option. The `algorithmNameTokens` rule has been refined to robustly capture complex algorithm names without relying on target-specific semantic predicates.

## Integration with grammars-v4

* `desc.xml`: This file is provided for integration with the `antlr/grammars-v4` testing infrastructure. It specifies Python 3 as the target language for this grammar and points to the example files located in the `examples/` directory.

## Origin and Testing

The grammar was developed based on:

1. Official Kumir documentation (DocBook XML format).
2. Extensive iterative testing against a suite of 60 example programs (from K.Y. Polyakov's collection).
3. Analysis of the behavior of the reference Kumir 2.1.0 IDE's parser/interpreter.
4. Collaborative refinement process.

The final version successfully parses all 60 provided test examples.

## Usage

These grammars are intended for use with the standard ANTLR v4 toolchain.

Target language example (Python 3):

```bash
antlr4 -Dlanguage=Python3 KumirLexer.g4 KumirParser.g4 -visitor -o output_dir