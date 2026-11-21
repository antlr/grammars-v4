# CCS Grammar

An ANTLR4 grammar for [Cascading Code Sheets](https://esolangs.org/wiki/CCS) files.

CCS is a CSS-inspired experimental programming language with expressions,
variables, reusable rules, and event-driven styles. It is not standard CSS.

This grammar supports numeric and string values, three- and six-digit hexadecimal
colors, `none`, bare style keywords, `$variable` references, arithmetic, and `%`,
`vw`, `vh`, and `s` units. Multiplication and division bind more tightly than
addition and subtraction. Parentheses and unary signs are supported.

Selectors include tags, classes, IDs, pseudo-classes and pseudo-elements, plus
child (`>`), adjacent sibling (`+`), general sibling (`~`), and alternative (`|`)
combinators. Style bodies can contain event bodies or nested rules. Statements
and declarations end in semicolons, including top-level rules. Assignments use
`=`, following the reference EBNF rather than the inconsistent `:=` shown in its
event example. Block comments use `/* ... */`.

The ten inputs in [`examples/`](examples/) cover these features. This is a syntax
parser, not a CSS or JavaScript compiler. The entry point is `stmts` and requires
complete input consumption.

Run all examples with `mvn -pl ccs clean test` from the repository root, or
`mvn clean test` from this directory. `desc.xml` configures the same entry point
for Trash Toolkit drivers.
