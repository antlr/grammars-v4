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
| `s` | `s` | zero or more whitespace/comment tokens |
| `rs` | `RS` | one or more whitespace/comment tokens |
| `comment` | `comment` | consumes a COMMENT token; lexer handles nesting |
| `cchar` | `cchar` | stub; part of COMMENT lexer rule |
| `whitespace_` | `whitespace` | consumes a WS token |

### Whitespace and comments

The iXML spec defines whitespace (`s`) and required separation (`RS`) as
grammar rules. `WS` and `COMMENT` stay on the default token channel so the
parser can enforce required separation between rules and between prolog
keywords. Comments, including nested comments, count as separation. Optional
spacing remains optional everywhere the specification uses `s`.

### Dots in names and quoted characters

Names are assembled from adjacent name-segment tokens and punctuation. Dots
are separate tokens: parser context distinguishes the dots in `a.b` or a
trailing-dot name `a.` from the rule terminator. Visible separators prevent
accidental joining of names across whitespace or comments. No target-specific
actions or predicates are required.

Quoted strings must contain at least one decoded character. Single-character
quoted tokens (including a doubled quote escape) are distinguished from longer
strings, so range endpoints accept exactly one character syntactically.

### Unicode

The `NAME` lexer rule uses ANTLR4 Unicode property escapes (`\p{L}`,
`\p{Nd}`, `\p{Mn}`) to match the namestart / namefollower character classes
defined in the iXML specification.  The `WS` rule uses `\p{Zs}` for Unicode
space separators.

## Example

`examples/ixml.ixml` is the self-describing iXML grammar from the
specification — the grammar for iXML written in iXML notation.

## Regression tests

With Java, Javac, Python 3, and an ANTLR complete JAR available:

```sh
python tests/run-tests.py /path/to/antlr4-4.13.2-complete.jar
```

This generates and compiles a Java parser in a temporary directory, exercises
valid and invalid grammar syntax, checks dotted names in the parse tree, and
parses the supported `.ixml` samples recursively. Both lexer and parser errors
are counted. As in `desc.xml`, the experimental `XPath.decorated.ixml` sample
is excluded because it contains non-iXML annotations.

The Maven test plugin is configured to scan `examples/` with the `.ixml`
extension filter. Its expected-error sidecar checks rejection of the annotated
XPath sample instead of treating that experimental syntax as valid iXML.
