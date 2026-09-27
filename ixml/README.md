# Invisible XML (iXML) Grammar

ANTLR4 grammar for [Invisible XML 1.0](https://invisiblexml.org/1.0/).

Invisible XML (iXML) is a language for treating any parseable format as XML.
An iXML grammar describes a syntax; any input conforming to that syntax can
be parsed and the result delivered as an XML document.

## Grammar structure

Parser rule names mirror those of the
[iXML 1.0 specification grammar](https://invisiblexml.org/1.0/#complete):

| ANTLR4 rule | iXML spec rule | Notes |
|---|---|---|
| `ixml` | `ixml` | top-level entry point |
| `prolog` | `prolog` | optional version declaration |
| `version` | `version` | `ixml version "1.0" .` |
| `rule_` | `rule` | renamed: `rule` is an ANTLR4 keyword |
| `mark` | `mark` | `@`, `^`, or `-` |
| `alts` | `alts` | alternation list |
| `alt` | `alt` | one alternative |
| `term_` | `term` | renamed: avoids Java keyword |
| `factor` | `factor` | atomic parsing expression |
| `repeat0` | `repeat0` | `*` or `** sep` |
| `repeat1` | `repeat1` | `+` or `++ sep` |
| `option` | `option` | `?` |
| `sep` | `sep` | separator in `++` / `**` |
| `nonterminal` | `nonterminal` | rule reference |
| `name` | `name` | identifier |
| `terminal_` | `terminal` | literal or charset |
| `literal` | `literal` | quoted or encoded |
| `quoted` | `quoted` | `"…"` or `'…'` literal |
| `tmark` | `tmark` | terminal mark (`^` or `-`) |
| `string_` | `string` | renamed: avoids Java keyword |
| `dchar` | `dchar` | stub — handled in lexer |
| `schar` | `schar` | stub — handled in lexer |
| `encoded` | `encoded` | `#hex` literal |
| `hex` | `hex` | hex digit sequence |
| `charset` | `charset` | inclusion or exclusion |
| `inclusion` | `inclusion` | `[…]` |
| `exclusion` | `exclusion` | `~[…]` |
| `set_` | `set` | character-class body |
| `member` | `member` | string, hex, range, or class |
| `range_` | `range` | `"a"-"z"` |
| `from_` | `from` | renamed: avoids Java keyword |
| `to_` | `to` | upper bound of range |
| `character` | `character` | single-char literal or hex |
| `class_` | `class` | renamed: avoids Java keyword |
| `code` | `code` | Unicode category code (e.g. `L`, `Zs`) |
| `insertion` | `insertion` | `+"…"` or `+#hex` |
| `s` | `s` | optional whitespace (stub; lexer-handled) |
| `rs` | `RS` | required separation (stub; lexer-handled) |
| `comment` | `comment` | stub; COMMENT lexer rule handles nesting |
| `cchar` | `cchar` | stub; part of COMMENT lexer rule |
| `whitespace_` | `whitespace` | stub; WS lexer rule |

### Whitespace and comments

The iXML spec defines whitespace (`s`) and required separation (`RS`) as
grammar rules. In this ANTLR4 grammar they are empty stubs: the `WS` lexer
rule sends all Unicode Zs characters, tabs, and newlines to the hidden
channel, and the `COMMENT` lexer rule handles `{ … }` comments (including
nesting) and also sends them to the hidden channel.

### Unicode

The `NAME` lexer rule uses ANTLR4 Unicode property escapes (`\p{L}`,
`\p{Nd}`, `\p{Mn}`) to match the namestart / namefollower character classes
defined in the iXML specification.  The `WS` rule uses `\p{Zs}` for Unicode
space separators.

## Example

`examples/ixml.ixml` is the self-describing iXML grammar from the
specification — the grammar for iXML written in iXML notation.
