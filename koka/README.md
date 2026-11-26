# Koka Grammar

[Koka](https://koka-lang.github.io/koka/doc/index.html) is a function-oriented
programming language with a signature feature: a full effect type and
handler system, which tracks and lets you compose the side effects
(exceptions, state, I/O, nondeterminism, ...) a function can have directly
in its type, and lets user code define and handle new effects.

## Source

[GitHub project](https://github.com/koka-lang/koka)

## Specification

[The Koka book](https://koka-lang.github.io/koka/doc/book.html)

## Notes

Koka's real surface syntax is layout-sensitive, in the same family as
Haskell's or Python's: an indented line implicitly opens a `{`, aligned
lines get an implicit `;`, and dedenting closes the block with an implicit
`}` (and Koka always inserts a `;` right before a `}`, explicit or
implicit). The compiler's own lexer (`doc/spec/grammar/lexer.l`) runs
exactly the same grammar either way, because every implicit layout token
has an always-valid, explicit spelling — `{`, `}`, and `;` can simply be
written out. This grammar targets that explicit form. Replicating Koka's
layout algorithm exactly would mean reimplementing its "continuation" and
"ending" token tables, which control precisely when an indented new line
is allowed to open an implicit block versus merely continuing the current
one — well beyond what is useful to encode in a static ANTLR grammar.

A second simplification: Koka's own binary operators are user-definable,
with their precedence and associativity set by `infix`/`infixl`/`infixr`
declarations rather than fixed by the grammar (`doc/spec/grammar/parser.y`
resolves this with its own built-in operator-precedence parser). A static,
context-free ANTLR grammar cannot honor a fixity table that is only known
at compile time, so this grammar instead combines every binary operator
application at a single, flat, left-associative level.

This grammar was built from the compiler's own formal lexer and parser
specifications (`doc/spec/grammar/lexer.l`, `doc/spec/grammar/parser.y`)
in the `koka-lang/koka` repository, together with the project's "book" and
`whatsnew.md` syntax changelog, and implements: module headers and
imports; `fun`/`fn`, `val`/`var`, `struct`, `type` (with constructors),
`alias`, and `effect` declarations; `infix`/`infixl`/`infixr` fixity
declarations; `if`/`then`/`elif`/`else` and `match`; `fn`/lambda
expressions; and Koka's effect-handling constructs (`effect` operation
declarations, `handler { ... }` values, `handle(e) { ... }` expressions,
and the `with` statement).

Entry rule: `program`
