# SETL grammar

[SETL](https://en.wikipedia.org/wiki/SETL) ("SET Language") is a very-high-level
programming language designed in the late 1960s by Jacob T. Schwartz at New
York University. It treats finite sets and tuples (sequences) as first-class
built-in data structures, and provides set-former/tuple-former
comprehensions (`{e : x in S | P}`), existential/universal quantifiers
(`exists`/`forall`), and set-theoretic operators (`union`, `inter`, `with`,
`less`, `in`, `subset`, ...) as primitive language constructs.

Reference: https://en.wikipedia.org/wiki/SETL

## Scope and simplifications

This grammar is based on the classical SETL language as described in
*Programming with Sets: An Introduction to SETL* (Schwartz, Dewar, Dubinsky
and Schonberg, 1986). Later SETL dialects and implementations (e.g. GNU SETL)
extended or altered parts of the surface syntax over the decades, and
surviving secondary material on the original language is sparse, so the
following deliberate simplifications were made:

- Programs are `program Name; ... end Name;` blocks containing variable/
  constant declarations (`var`/`const`), nested `procedure` declarations,
  and statements. This grammar does not model SETL's separate-compilation
  / package units.
- Statements: `if`/`elseif`/`else`/`end if`, `while`/`loop`/`end loop`,
  `for x in S [| cond] loop ... end loop`, `case ... of alt => ... ;
  otherwise => ... ; end case`, `return`, `print(...)`, and assignment
  (`:=`).
- Expressions include set formers (`{e : x in S | P}`), tuple formers
  (`[e : x in S | P]`), set/tuple literals (`{1,2,3}`, `[1,2,3]`), ranges
  (`a..b`), cardinality (`#S`), and the quantifiers `exists`/`forall`.
- Line comments use `$` to end of line, and `/* ... */` block comments are
  also accepted, matching the classical language reference; this grammar
  does not attempt to support every comment convention used by later SETL
  implementations.
- Procedure parameters are by-name only (no explicit mode annotations like
  `rd`/`wr`/`rw`, which some SETL implementations support); arbitrary
  nesting of expressions and statements is supported, but some of the
  language's less common built-in operators (e.g. `npow`, `pow`, `domain`,
  `range` as operators rather than identifiers) are left as ordinary
  function-call syntax (`domain(f)`) rather than dedicated operators.

Entry rule: `program`.

## Examples

The `examples/` directory contains 10 SETL programs exercising: a basic
`print` statement, set literals and operators (`union`/`inter`/`less`/
`with`/`#`), tuple literals and indexing/slicing, set- and tuple-formers,
the `exists`/`forall` quantifiers, `if`/`elseif`/`else`, a `while` loop, a
`for` loop with a filter condition, a `case` statement, and a recursive
`procedure` (factorial) called from the main program.
