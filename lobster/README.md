# Lobster Grammar

[Lobster](https://aardappel.github.io/lobster/) is a statically typed, lexically
scoped, garbage collected (reference counted), embeddable scripting/systems
language with a focus on game development, written in C++ by Wouter van Oortmerssen.

## Source

[GitHub project](https://github.com/aardappel/lobster)

## Specification

[Language reference](https://aardappel.github.io/lobster/language_reference.html)

## Notes

Lobster uses significant indentation (like Python) rather than braces or `end`
keywords to delimit the bodies of functions, classes/structs, and control-flow
statements (`if`/`elif`/`else`, `while`, `for`, `switch`, `guard`). This grammar
follows the convention used elsewhere in this project for such languages (see
e.g. `python/python3` and `gdscript`): the lexer grammar (`LobsterLexer.g4`)
is paired with a small hand-written base lexer class, `LobsterLexerBase`
(provided per-target under `Java/` and `CSharp/`), which synthesizes `INDENT`
and `DEDENT` tokens whenever the indentation level of a logical line changes,
the same technique used by CPython's own tokenizer.

The language reference's own EBNF is terse and leaves a few points
underspecified or inconsistent (operator associativity, the precedence of
`and`/`or` relative to the bitwise operators, the grammar for operator
overload declarations, etc). This grammar resolves those by using a
conventional, unambiguous left-to-right precedence ladder for binary
operators, and by restricting trailing function-value ("block") arguments
and inline `fn` lambdas to a single-line body (named `def` functions and the
control-flow statements still support full, multi-line indented bodies).

Entry rule: `program`

## Reference

* [pldb](http://pldb.info/concepts/lobster)
