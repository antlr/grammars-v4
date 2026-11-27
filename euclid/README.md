# Euclid Grammar

[Euclid](https://en.wikipedia.org/wiki/Euclid_(programming_language)) is a
Pascal-family systems programming language designed in the mid-1970s at
the University of Toronto (Butler Lampson, James Mitchell, Jim Horning,
Ralph London, and Gerald Popek, implemented by Ric Holt and James Cordy),
specifically to support formal verification of programs. It is the
ancestor of Concurrent Euclid and of Turing.

## Source

[Wikipedia: Euclid (programming language)](https://en.wikipedia.org/wiki/Euclid_(programming_language))

## Specification

B.W. Lampson, J.J. Horning, R.L. London, J.G. Mitchell, G.J. Popek,
"Report on the Programming Language Euclid", *ACM SIGPLAN Notices* 12(2),
February 1977, pp. 1-79, is the authoritative language report.

## Notes

Euclid is obscure enough, and old enough, that no usable primary-source
syntax reference could be found while writing this grammar: the 1977
language report above is behind the ACM Digital Library paywall with no
free mirror found, and the Wikipedia article (the only page actually
readable) describes the language's design goals and restrictions but
gives no example code or formal syntax at all. This grammar is instead
reconstructed from what Wikipedia *does* confirm about Euclid — routines
are closed scopes with no side effects, must explicitly declare their
`imports` (and `exports`) rather than relying on global or aliased state,
have no `goto`, no nested functions, and (notably) **no floating-point
numbers** — together with ordinary Pascal-family syntax, which Euclid is
explicitly descended from and which this grammar's `begin`/`end`,
`record`, pointer/`collection`, and `for`/`while`/`case` forms follow.

Treat this grammar as a plausible, workable reconstruction of Euclid
syntax rather than a verified transcription of the 1977 report —
corrections against that report, "The Euclid Language: a progress
report" (Holt, Wortman, Cordy, Crowe, 1978), or any surviving Euclid
source code would be welcome. One Euclid feature this grammar does not
attempt, for lack of any syntax detail to go on, is variant/union record
types.

Entry rule: `program`
