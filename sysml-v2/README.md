# SysML v2 — ANTLR4 Grammar

ANTLR4 grammar for the [SysML v2](https://www.omg.org/spec/SysML/2.0) textual
notation, automatically generated from the OMG specification grammar (KEBNF
format).

## Source

- **Specification**: [Systems-Modeling/SysML-v2-Release](https://github.com/Systems-Modeling/SysML-v2-Release)
- **Release tag**: `2026-01`
- **Generator**: [daltskin/sysml-v2-grammar](https://github.com/daltskin/sysml-v2-grammar)

## Grammar Structure

| File | Description |
|------|-------------|
| `SysMLv2Lexer.g4` | Lexer grammar — keywords, operators, literals, whitespace |
| `SysMLv2Parser.g4` | Parser grammar — full SysML v2 textual syntax |

## Entry Point

The start rule is `rootNamespace`.

## License

MIT — Copyright (c) 2026 J Dalton

The SysML v2 specification grammar is owned by the Object Management Group
(OMG).  This project provides a derived ANTLR4 translation of the official
KEBNF grammar.

## Reference

- <http://pldb.info/concepts/sysml>
