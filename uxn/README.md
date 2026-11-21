# Uxntal Grammar

An ANTLR4 grammar for [Uxntal](https://wiki.xxiivv.com/site/uxntal.html), the
concatenative assembly language of the [Uxn](https://wiki.xxiivv.com/site/uxn.html)
virtual machine. The grammar follows the formal EBNF published at
[uxntal.bnf.txt](https://wiki.xxiivv.com/etc/uxntal.bnf.txt).

A Uxntal program is a whitespace-separated sequence of words. The leading
rune of a word selects its category:

- `@parent` / `&child` — labels
- `$relative` / `|absolute` — padding
- `#literal` — a literal hex byte or short, pushed via an implicit `LIT`
- `.zero-page` / `-raw zero-page`, `,relative` / `_raw relative`,
  `;absolute` / `=raw absolute` — the six addressing-mode literal/raw pairs
- `"raw string` — a raw ASCII string (not whitespace-delimited beyond the word)
- `!jmi` / `?jci` — immediate jump and conditional-jump runes
- `( comment )`, `{ anonymous label }`, `% macro { ... }`, and the
  `[ ]` / stray `)` bracket runes used purely for readability

A bare word with no leading rune is either an opcode (an uppercase mnemonic
from the 32 standard operations plus `LIT`, with any combination of the `2`
short, `k` keep, and `r` return mode suffixes), an immediate macro/label
invocation, or raw hex data.

All ten files in [`examples/`](examples/) are tested with the `program`
entry point, which consumes the entire input. Run `mvn -pl uxn clean test`
from the repository root, or `mvn clean test` from this directory. `desc.xml`
configures the same entry point for Trash Toolkit drivers.
