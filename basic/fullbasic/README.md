# Full BASIC

An ANTLR4 grammar for **Full BASIC**, the language defined by:

* ANSI X3.113-1987, *Programming Languages - Full BASIC*, as adopted by the US Government in
  FIPS PUB 68-2 (1987);
* ECMA-116, *BASIC*, 1st edition, June 1986;
* ISO/IEC 10279:1991, *Information technology - Programming languages - Full BASIC*.

Section numbers (`§`) in the grammar comments refer to ANSI X3.113-1987.

The grammar is split in a lexer (`FullBASICLexer.g4`) and a parser (`FullBASICParser.g4`). The entry rule is
`program`. It has no actions or predicates, so it works with every ANTLR target. The lexer uses
`caseInsensitive = true` (ANTLR 4.10 or later), because keywords and identifiers are case-insensitive.

## Modules covered

* Core (programs, numbers, strings, arrays, control flow, procedures, input/output, `DATA`/`READ`)
* Enhanced files (sequential, stream, relative and keyed files; internal, display and native record types; `ASK`/`SET` file attributes; `TEMPLATE`)
* Exception handling (`WHEN EXCEPTION IN`, `HANDLER`, `USE`, `RETRY`, `CONTINUE`, `CAUSE EXCEPTION`)
* Graphics (`PLOT`, `DRAW`, `PICTURE`, `SET WINDOW`, `ASK` graphics state, transformations)
* Fixed decimal (`OPTION ARITHMETIC DECIMAL`, `DECIMAL(n,m)` and the decimal functions)

The ECMA-116 differences (for example the `EXTYPE`/`EXLINE` exception functions and ECMA-only forms) are
included.

A program is a list of numbered lines. Line numbers, line continuation (`&`), the `IMAGE:` mode of `PRINT USING`
and `REM`/`!` comments are handled in the lexer.

## Deviations from the standard text

The grammar accepts a superset of a few productions, on purpose, to keep the grammar free of actions and
predicates. Semantic checks are left to the consumer.

* **`DATA`** is parsed by the parser, not by the lexer. A data item is a quoted string or a sequence of
  unquoted tokens (`unquoted_token`), so that `DATA A B, 1E5, -3` is accepted. Whether an unquoted datum is a
  valid number or string (§4.3 and §14) is not checked by the grammar. Because `REM ` followed by a space
  opens a remark, a datum such as `DATA A REM B` is lexed as a remark.
* **`POINTER`** is accepted in front of `RECORD` and `KEY` (`POINTER RECORD n`, `POINTER KEY ...`) as well as
  in front of `BEGIN`, `END`, `NEXT` and `SAME` (§11.2.2). The standard writes `RECORD` and `KEY` without
  `POINTER`; the grammar accepts both forms, so no valid program is rejected.
* **External picture definitions** (`EXTERNAL PICTURE ... END PICTURE`, §13.5.2) are parsed like external
  procedures (rule `external_picture_def`, a `procedure` alternative). This is a local simplification of the
  program-unit structure.
* **Supplied function names and keywords as identifiers.** In Full BASIC the names of the supplied functions
  (`SIN`, `LEN`, `TAB`, `CHR$`...) and most keywords are not reserved, so they can also be used as identifiers
  (rules `soft_syntax` and `soft_syntax_str`). A supplied function name followed by an argument list is
  always parsed as a function call (the call alternative is tried first in `primary` and `string_primary`);
  a bare name is an identifier. Telling a user array named like a supplied function apart needs scope
  information, which is left to the consumer.
* **Implementation-defined characters.** Inside `DATA` and in strings, the Latin-1 characters from `0x80` to
  `0xFF` are accepted (the lexer token `OTHER_CHARACTER`), because the standard leaves the character set
  beyond the basic one implementation defined. The example programs are plain ASCII.

## Examples

`examples/` has programs that the grammar must accept without errors. They come from the Full BASIC
Conformance Suite and keep their original names (for example `PRT-0001.BAS` is a printing test). The
programs that test syntax errors (`REM TYPE REJECT`) are not included. Their `REM` header lines tell the
claim of the standard they test.

## License

The grammar and the examples are licensed under the MIT license.

* Grammar copyright (c) 2026 AJGB (ajgb.eu)
* Conformance suite: https://github.com/Full-BASIC/full-basic-conformance

The conformance suite is interpreter independent. It has about 2,000 programs with their expected results,
claim by claim, for ANSI X3.113-1987 and ECMA-116.
