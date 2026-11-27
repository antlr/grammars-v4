# occam Grammar

[occam](https://en.wikipedia.org/wiki/Occam_(programming_language)) is a
concurrent programming language based on Tony Hoare's Communicating
Sequential Processes (CSP), designed by INMOS for its transputer
microprocessors in the early 1980s. Its defining features are explicit
sequential (`SEQ`) and parallel (`PAR`) composition of processes, and
message-passing communication over typed channels (`!` to output, `?` to
input) instead of shared variables or locks.

## Source

[Wikipedia: occam (programming language)](https://en.wikipedia.org/wiki/Occam_(programming_language))

## Specification

occam 2 (the dialect this grammar targets) was specified in INMOS's
*occam 2 Reference Manual*.

## Notes

Like Python, occam uses significant indentation instead of braces or
`BEGIN`/`END` keywords to delimit the bodies of `SEQ`, `PAR`, `ALT`, `IF`,
`WHILE`, `CASE`, and `PROC`/`FUNCTION` (real occam requires exactly 2
spaces per indentation level; this grammar, like the other indentation-
sensitive grammars in this project, tracks indentation changes generically
rather than enforcing an exact column width). It follows the same
INDENT/DEDENT technique used by `python/python3`, `gdscript`, and
`lobster` elsewhere in this project: the lexer grammar (`OccamLexer.g4`)
is paired with a small hand-written base lexer class, `OccamLexerBase`
(provided per-target under `Java/` and `CSharp/`), which synthesizes
`INDENT` and `DEDENT` tokens whenever the indentation level of a logical
line changes.

A declaration (`INT x:`, `CHAN OF INT c:`, a `PROC`, and so on) scopes
over the single process that textually follows it — this is standard
occam: a sequence of declarations is typically followed by one final
process (very often a `SEQ`, used explicitly whenever more than one
statement needs to run in order, since every construct's body is exactly
one process).

Entry rule: `program`
