# SIMSCRIPT Grammar

[SIMSCRIPT](https://en.wikipedia.org/wiki/SIMSCRIPT) is a general-purpose,
English-like discrete-event simulation programming language originally
developed at RAND Corporation in 1962-63. It was substantially extended as
SIMSCRIPT II and then SIMSCRIPT II.5, commercialized by CACI (the company
RAND co-founders Herb Karr and Harry Markowitz started to productize it) and
released in 1971. SIMSCRIPT programs model a simulated world as entities,
attributes, and sets, and define behavior as events and processes scheduled
on a simulated clock.

## Source

https://en.wikipedia.org/wiki/SIMSCRIPT

## Specification

No freely-available, authoritative SIMSCRIPT II.5 language reference manual
could be located online -- CACI's current corporate site no longer hosts one
(`https://www.caci.com/simscript` returns HTTP 404), and the canonical
language references are print-only books: Philip J. Kiviat's *Simscript
II.5: Programming Language* (CACI, 1973) and C. Russell's *Building
Simulation Models with SIMSCRIPT II.5* (CACI, 1983), neither of which has a
freely accessible full-text copy online. The Wikipedia article above is the
best available secondary source and was used instead:

https://en.wikipedia.org/wiki/SIMSCRIPT

## Notes

Because the authoritative manuals above were not accessible, this grammar
does not claim to be a verified transcription of SIMSCRIPT II.5's full
grammar. SIMSCRIPT II.5 is well known for allowing many interchangeable,
English-like phrasings of the same construct (several equivalent word
orders for a loop, or for a PRINT statement with formatting clauses); this
grammar instead models one fixed, unambiguous word order per construct:

- A program is an optional `PREAMBLE ... END` block (entity/attribute
  declarations via `EVERY <entity> HAS A/AN <attribute>, ...`, `PROCESSES
  INCLUDE ...`, `EVENT NOTICES INCLUDE ...`, and `DEFINE ... AS ...
  VARIABLE` declarations), followed by a mandatory `MAIN ... END` block,
  followed by zero or more `ROUTINE`/`PROCESS`/`EVENT TO <name> [GIVEN
  params] ... END` declarations.
- Statements: `LET`, `ADD ... TO ...`, `SUBTRACT ... FROM ...`,
  `IF ... [ELSE ...] ALWAYS`, `FOR EACH ... IN ... [WITH ...] DO ... LOOP`,
  `WHILE ... DO ... LOOP`, `SCHEDULE`, `CAUSE`, `CALL`, `CREATE`, `DESTROY`,
  `PRINT`, `RETURN`, `STOP`, and `WORK ... UNITS`.
- Attribute/entity/variable names use underscores (`ARRIVAL_TIME`) rather
  than the dotted compound names SIMSCRIPT II.5 traditionally allows
  (`ARRIVAL.TIME`), to keep the lexer's identifier rule unambiguous.
- Comments use `!!` to end of line; this is a deliberate choice (not a
  verified historical SIMSCRIPT convention) made to avoid lexical
  ambiguity with the `*` multiplication operator, since no single
  comment convention for SIMSCRIPT II.5 could be confirmed from the
  sources above.
- `PRINT` is modeled as a simple expression-list statement rather than
  SIMSCRIPT II.5's full `PRINT 1 LINE WITH ... AS FOLLOWS` formatted-output
  syntax.

Entry rule: `program`.

## Examples

The `examples/` directory contains 10 SIMSCRIPT programs exercising: a
basic `PRINT` statement, a `PREAMBLE` with entity/attribute/process/event
declarations, `LET` assignment with arithmetic, `ADD`/`SUBTRACT`,
`IF`/`ELSE`/`ALWAYS`, a `FOR EACH ... WITH ... DO ... LOOP`, a
`WHILE ... DO ... LOOP`, `SCHEDULE`/`CAUSE` with an `EVENT` routine,
`CREATE`/`DESTROY`, and a `ROUTINE` with a `GIVEN` parameter and `RETURN`
value invoked with `CALL`.
