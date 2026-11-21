# Nock Grammar

An ANTLR4 grammar for [Nock](https://developers.urbit.org/reference/nock/definition),
the low-level, functional, combinator-based virtual machine language that underlies
[Urbit](https://urbit.org/).

Nock operates on *nouns*: an atom (an unsigned integer) or a cell, an ordered pair
of nouns written `[a b]`. Bracketed sequences of three or more nouns are
right-associative, so `[a b c]` is shorthand for `[a [b c]]`. Nock's interpreter
function itself, `*[subject formula]`, reduces a formula (opcodes `0` through `11`)
against a subject noun. The spec also defines dot-notation shorthand for frequently
used reductions: `?` (is-cell test), `.+` (increment, written with a leading dot to
avoid ambiguity with a signed numeral), `=` (equality test), `/` (tree/slot
addressing), `#` (tree edit), and `*` (nock itself). Each of these operators is
applied to a single noun argument, which is conventionally written as a bracketed
cell.

This grammar parses a single Nock noun — an atom, a cell, or one of the
dot-notation operator forms applied recursively to nouns. It covers the surface
syntax of Nock exactly as given in the Nock 4K specification; it does not evaluate
or reduce formulas.

All ten files in [`examples/`](examples/) are tested with the `nock` entry point,
which consumes the entire input. Run `mvn -pl nock clean test` from the repository
root, or `mvn clean test` from this directory. `desc.xml` configures the same entry
point for Trash Toolkit drivers.
