# Minimal BASIC (ECMA-55) grammar

ANTLR 4 grammar for Minimal BASIC as defined by ECMA-55, 1st edition (January 1978),
equivalent to ANSI X3.60-1978. Start rule: `program`. License: MIT.

## Notes

- Case-insensitive (`caseInsensitive = true`), as a convenience: ECMA-55 only
  defines upper-case letters.
- Lowercase is accepted everywhere, including inside strings, remarks and DATA
  (interpreter convenience; ECMA-55 §4 only has upper case, NBS P204/P205 test this).
- `BAD_STRING` exists only to improve error positions: it matches a quoted string with
  characters outside §4.2 or without a closing quote; no parser rule uses it.
- Tabs, a UTF-8 BOM or any other non-§4 character lex as `ERROR_CHAR`.
- Spaces go to the hidden channel. §5.4 rules about spaces around keywords, leading
  spaces and the 72-character line limit are not checked by the grammar.
- `GO TO` and `GO SUB` are single tokens (`GO space* TO`, §12.2).
- `DATA` switches to a lexer mode, so unquoted strings keep their inner spaces (§4.2).
- The sign belongs to `numeric-expression`, not to `primary` (§8.2): `2^-1` is rejected.
- Not checked: line order and range (1-9999), END as the last line, FOR/NEXT
  pairing, whether a call to a user function matches its DEF (with or without a
  parameter), OPTION BASE value (0 or 1).

## Tests

- `examples/*.BAS`, except `invalid_*`: 134 programs from the public-domain NBS
  Minimal BASIC test suite (NBSIR 77-1420) that must be accepted. Not included: the
  72 NBS programs that test syntax errors, and P204/P205, which test lower case
  (outside §4; this grammar accepts them).
- `examples/invalid_*.BAS` (test `Negative` in `desc.xml`): 13 small programs that
  must be rejected, with the expected messages in `.errors`, for example `2^-1`,
  `A$<B$`, `SIN(1,2)`, `LET A$=B$+C$` or a character outside the §4 set.
- `ON ... GOSUB` is also rejected (§12 only has `ON ... GO TO`), but it has no
  negative test: the error follows a numeric expression, and the Java runtime
  reports a larger set of expected tokens there than the other targets.
