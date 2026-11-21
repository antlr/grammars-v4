# Well-known text (WKT) Grammar

Well-known text (WKT) is a text markup language for representing vector geometry objects on a map, spatial reference systems of spatial objects and transformations between spatial reference systems. A binary equivalent, known as well-known binary (WKB), is used to transfer and store the same information on databases. The formats were originally defined by the Open Geospatial Consortium (OGC) and described in their Simple Feature Access and Coordinate Transformation Service specifications. The current standard definition is in the ISO/IEC 13249-3:2016 standard, "Information technology — Database languages — SQL multimedia and application packages — Part 3: Spatial" (SQL/MM) and ISO 19162:2015, "Geographic information — Well-known text representation of coordinate reference systems".

## Reference

The grammar implements subclause 5.1.67, `<well-known text representation>`, and covers all 23 geometry types it defines.

The source document is **ISO/IEC DIS 13249-3, 5th edition** — the Draft International Standard circulated for ballot as
ISO/IEC JTC 1/SC 32 N 2593, dated 2015-01-25, at document stage 40 (Enquiry) and numbered `ISO/IEC 13249-3:201x(E)`
internally. That draft, and not the published ISO/IEC 13249-3:2016, is what this grammar was written against: the published
standard is paywalled, the ballot text is not. The two are expected to agree on the WKT BNF, but that has not been checked
against the published text, so where they differ, assume the error is here rather than in the standard.

The ballot text is not self-consistent everywhere. The grammar resolves the discrepancies in 5.1.67 as follows:

| Discrepancy in the draft | Resolution |
| --- | --- |
| `<compoundsurface text body>` is referenced by `<surface text>` and `<shell text>` but never defined | Read as `<compoundsurface text>`, matching every other `text body` production |
| `<angle text>` is cross-referenced to subclause 15.1.21 | The definition is in 16.1.21, `<angle text representation>` |
| `<vector text representation>` is cross-referenced to subclause 16.2.22 | The definition is in 17.2.22, the `<well-known text representation>` of an `ST_Vector` |

## Deviations from the standard

**A `SPIRALTYPE` value may contain anything but a comma or a parenthesis.** The standard defines `<spiraltype text>` as
free-form `<letters>`, and 5.1.68 length-prefixes the value in the binary representation, so the value set is open — 4.2.12
lists `clothoid`, `bloss`, `biquadratic`, `sine` and `cosine` only as the *initial* set. Text has no length prefix, and
`<letters>` admits the very characters that would end the value, so the lexer instead reads a spiral type up to the comma or
closing parenthesis that terminates it. Interior spaces are preserved, so `SPIRALTYPE Wiener Bogen` parses. A name containing
a comma or a parenthesis is the one case this grammar cannot represent.

This is why the grammar is split into `wktLexer.g4` and `wktParser.g4`: reading the value that way needs a lexer mode, and
ANTLR permits modes only in a standalone lexer grammar.

**The OGC Simple Features forms are accepted alongside the ISO forms** for the three types where the two differ. Dropping the
OGC forms would reject data that existing implementations emit.

| Type | ISO form | OGC form, also accepted |
| --- | --- | --- |
| `TRIANGLE` | `TRIANGLE (0 0, 1 0, 0 1)` | `TRIANGLE ((0 0, 1 0, 0 1, 0 0))` |
| `POLYHEDRALSURFACE` | `POLYHEDRALSURFACE Z (PATCHES (POLYGON Z ((...))))` | `POLYHEDRALSURFACE Z (((...)))` |
| `TIN` | `TIN Z (PATCHES ((...)) ELEMENTS (...) MAXSIDELENGTH 5)` | `TIN Z (((...)))` |

**`CLOTHOID` accepts two forms.** Alongside the standard's `<clothoid text>`, the grammar keeps the `(k0, k1, L)` form
introduced for JTS by [CLOTHOID_PROPOSAL.md](CLOTHOID_PROPOSAL.md), in which the start point, tangent and curvature are
inherited from the preceding segment of the enclosing `COMPOUNDCURVE`. The two are told apart by the token after the opening
parenthesis: a number for the JTS form, `REFERENCELOCATION` for the ISO one.

| Form | Example |
| --- | --- |
| ISO 13249-3 | `CLOTHOID (REFERENCELOCATION AFFINEPLACEMENT (...), SCALEFACTOR 100, STARTDISTANCE 0, ENDDISTANCE 50)` |
| JTS extension | `COMPOUNDCURVE ((0 0, 100 0), CLOTHOID (0, 0.005, 80))` |

**Three further extensions are accepted** that the standard does not define: the `LINEARRING` type, the ordinate values `NaN`,
`Inf` and `-Inf`, and unparenthesised points in `MULTIPOINT`, as in `MULTIPOINT (1 2, 3 4)`.
