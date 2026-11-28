# Ratfor grammar

[Ratfor](https://en.wikipedia.org/wiki/Ratfor) ("Rational Fortran") is a
preprocessor language designed by Brian Kernighan in the mid-1970s, described
in the book *Software Tools* by Kernighan and Plauger. Ratfor adds C-like
structured control flow -- `if`/`else`, `while`, `for`, `repeat`/`until`,
`break`, `next` -- and free-form syntax (statements on one line, braces for
grouping, `#` comments) on top of plain Fortran 66/77, and compiles down to
ordinary Fortran source.

Reference: https://en.wikipedia.org/wiki/Ratfor

## Scope and simplifications

Ratfor embeds arbitrary Fortran statements verbatim wherever a statement is
expected, and the full Fortran 66/77 statement grammar (every I/O edit
descriptor, `FORMAT` string, `EQUIVALENCE`/`COMMON` storage layout rule,
`DATA` initializer, etc.) is large and only loosely specified by surviving
secondary sources. This grammar focuses on Ratfor's own contribution -- the
control-flow and free-form syntax -- and supports the subset of embedded
Fortran statements that appear in idiomatic Ratfor programs:

- Assignment (`x = expr`), including array-element assignment (`a(i) = expr`).
- `call name(...)`, `return`, `return(expr)`, `stop`.
- `print *, ...`, `read(*, *) ...`, `write(*, *) ...` (list-directed I/O only
  -- no `FORMAT`-string edit descriptors).
- Simple type declarations: `integer`, `real`, `double precision`,
  `logical`, `character` (with optional array dimensions), plus standalone
  `dimension` and `common` statements.
- `subroutine name(...) ... end` and `[type] function name(...) ... end`.
- Numeric labels (`10: stmt`) in the generalized `label: statement` form;
  this grammar does not attempt to model Fortran's column-based fixed-form
  label field, since Ratfor source is free-form.
- `double precision` is matched as the two-word literal with exactly one
  space between the words.
- A line ending in `\` joins with the next line (Ratfor's own continuation
  convention); beyond that, this grammar treats all whitespace (including
  newlines) as insignificant, so statement separation relies on `;`, braces,
  and the fact that most statements are unambiguously terminated by their
  own syntax -- it does not model Ratfor's column-70 fixed-form continuation
  rule inherited from Fortran.
- C-like relational/logical operators (`==`, `!=`, `<`, `>`, `<=`, `>=`,
  `&`, `|`, `!`, `&&`, `||`) are supported, since Ratfor is well known for
  adding these over Fortran's `.EQ.`/`.AND.`/etc. forms.

Entry rule: `program`.

## Examples

The `examples/` directory contains 10 Ratfor programs exercising: a basic
print statement, `if`/`else if`/`else`, a `while` loop, a C-like `for` loop,
a Fortran-style `do` loop, `repeat`/`until`, `break`/`next`, a `subroutine`
definition and call, a recursive `function`, array declarations via
`dimension`, and list-directed `read`/`write`.
