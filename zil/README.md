# ZIL

This directory contains an ANTLR 4 parser grammar for ZIL (Zork Implementation
Language), the language used to write classic Infocom interactive fiction.

The grammar is in [`zil.g4`](zil.g4). Ten small, tested examples are in
[`examples/working/`](examples/working/); larger historical source samples are
also kept in [`examples/`](examples/).

Language references:

- [ZIL on IFWiki](https://www.ifwiki.org/ZIL)
- [ZIL Language Guide](https://foss.heptapod.net/zilf/zilf/-/blob/branch/default/doc/ZIL%20Language%20Guide.odt?ref_type=heads)
- [MDL Language reference](https://mdl-language.readthedocs.io/en/latest/)

Semicolons comment out the following expression (an atom, string, list, or form),
not the rest of the line. Commented expressions remain represented in the parse
tree.

Run all example parser tests, including the historical samples and the ten
focused examples, from this directory with `mvn test`.
