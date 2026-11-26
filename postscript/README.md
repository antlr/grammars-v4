# PostScript Grammar

[PostScript](https://en.wikipedia.org/wiki/PostScript) is Adobe's
stack-based page description and programming language, introduced in 1982
and still the lingua franca of printers and imaging systems (and the
ancestor of PDF).

## Source

[Adobe PostScript Language Reference](https://www.adobe.com/content/dam/acom/en/devnet/actionscript/articles/PLRM.pdf)
(the "Red Book", 3rd edition)

## Specification

See the Adobe reference above, chapter 3 ("Language").

## Notes

PostScript's syntax is unusually simple and uniform for a general-purpose
language: a program is just a sequence of *objects* — numbers, strings,
names, procedures delimited by `{ }`, and arrays delimited by `[ ]` —
pushed onto (or, for executable objects, run against) an operand stack.
Control structures (`if`, `ifelse`, `for`, `repeat`, `loop`, `forall`),
definitions (`def`), and every arithmetic/stack/dictionary operator
(`add`, `dup`, `exch`, ...) are **not** reserved words and have no special
grammar productions of their own: they are ordinary executable names,
looked up in the current dictionary stack and invoked at run time. A
syntax grammar for PostScript therefore needs no keyword list and no
statement forms beyond "object" — control flow is encoded entirely as
data (procedure objects) consumed by otherwise perfectly ordinary
operator names.

This grammar implements the lexical level fully: integers, reals
(including exponent forms like `1.5e3`), radix numbers (`16#FF`), literal
strings (with nested, balanced parentheses and backslash escapes),
hexadecimal strings (`<...>`), ASCII-85 strings (`<~...~>`), literal names
(`/name`), immediately-evaluated names (`//name`), executable names, and
both `%`-comment forms (ordinary comments and DSC structuring comments
such as `%!PS-Adobe-3.0` and `%%EOF`, which carry no syntactic weight here
either).

Entry rule: `program`
