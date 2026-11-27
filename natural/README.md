# NATURAL Grammar

[NATURAL](https://www.softwareag.com/en_corporate/platform/adabas-natural.html)
is Software AG's fourth-generation programming language (4GL), long used
together with the ADABAS database for business application development
on mainframes, and still actively documented and supported today.

## Source

[Software AG](https://www.softwareag.com/)

## Specification

[Software AG documentation portal](https://documentation.softwareag.com/)
(see the NATURAL "Statements" and "Programming Guide" manuals)

## Notes

Software AG's documentation site returned HTTP 404 for every specific
NATURAL documentation page found via web search while writing this
grammar (search results confirmed the pages exist and summarized their
titles/content, but fetching the pages themselves consistently failed),
so this grammar could not be checked against the primary reference in
the detail the other grammars in this project were. It targets NATURAL's
modern "structured mode" (as opposed to the older, line-numbered
"reporting mode" inherited from NATURAL's BASIC-like origins): every
program begins with a `DEFINE DATA LOCAL`/`PARAMETER`/`GLOBAL` block
declaring level-numbered, `#`-prefixed fields with a format letter and
length in parentheses (e.g. `1 #NAME (A20)`, `1 #AMOUNT (N5.2)`), closed
by `END-DEFINE`; every subsequent block statement (`IF`, `FOR`, `REPEAT`,
`DECIDE ON`, `FIND`, `READ`, subroutine definitions) is closed by its own
`END-xxx` keyword; and the module as a whole is terminated by a final,
mandatory `END`.

Two scope limitations worth noting: real NATURAL also supports an older
line-numbered reporting mode and a full-line comment convention (a `*` as
the very first character of a line) that depends on column position —
this grammar, like the other position-independent grammars in this
project, does not track column position, so only the `/* ...` inline
trailing-comment form is implemented. Level numbers in `DEFINE DATA` are
accepted but their grouping/nesting semantics are not enforced
structurally (fields are parsed as a flat list).

Entry rule: `program`
