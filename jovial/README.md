# JOVIAL Grammar

JOVIAL ("Jules' Own Version of the International Algorithmic Language") is
an ALGOL-58-derived, block-structured systems/embedded programming
language designed at System Development Corporation by Jules Schwartz,
starting in 1959, and long used in US military and aerospace software
(radar, flight, and command-and-control systems). It was standardized as
MIL-STD-1589, revised as MIL-STD-1589C (the "J73" dialect).

## Source

[Wikipedia: JOVIAL](https://en.wikipedia.org/wiki/JOVIAL)

## Specification

MIL-STD-1589C is the authoritative specification for the language.

## Notes

JOVIAL is old and obscure enough that a primary-source language reference
was not available while writing this grammar: the MIL-STD-1589C PDF
mirrored at everyspec.com is image-only (no extractable text layer, so it
could not be read), and the DTIC-hosted copy of the Softech "Computer
Programming Manual for the JOVIAL (J73) Language" refused the request
(HTTP 403). This grammar is instead built from the one verbatim JOVIAL
example reproduced in the Wikipedia article (a `PROC RETRIEVE` procedure,
itself quoted from the J73 manual) together with JOVIAL characteristics
that are well attested across secondary sources: `BEGIN`/`END` blocks;
`ITEM`, `TABLE`, `DEF`, `CONSTANT`, and `STATUS`-enumerated declarations;
`PROC` procedures with colon-separated parameter lists; and the
"`FOR var : initial BY step WHILE condition;`" loop form. Two points this
grammar had no primary source for, and so resolves with an ordinary
ALGOL-family reading instead: operator precedence (a conventional
left-to-right ladder), and comment/string delimiters (comments in `"..."`,
string literals in `'...'`).

Treat this grammar as a reasonable, workable approximation of JOVIAL
syntax rather than a verified transcription of MIL-STD-1589C — corrections
against the actual standard (or the J73 manual) would be welcome.

Entry rule: `program`
