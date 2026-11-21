# GQL Grammar

ANTLR4 grammar for Graph Query Language (GQL), standardized as
ISO/IEC 39075:2024.

GQL is a declarative language for defining, querying, and modifying
property graphs.

## Scope

`GQL.g4` contains the lexer and parser rules. The parser entry point is
`gqlProgram`.

The grammar covers language constructs including:

- Session and transaction management.
- Graph and schema operations.
- Graph pattern matching.
- Filtering, projection, aggregation, ordering, and pagination.
- Data insertion, modification, and deletion.
- Procedure calls and expressions.

Parsing checks syntactic structure. It does not execute statements or
perform the semantic validation required by a database implementation,
such as resolving names, checking types, or enforcing access permissions.

Successful parsing does not by itself establish full conformance to
ISO/IEC 39075:2024.

## Origin and Attribution

### Original grammar

This grammar originates from the
[OpenGQL grammar project](https://github.com/opengql/grammar).

Credit for the original grammar belongs to the OpenGQL project and its
contributors.

The upstream project distributes its work under the
[Apache License, Version 2.0](https://github.com/opengql/grammar/blob/main/LICENSE).

### Integration into grammars-v4

[Miotto Nicola](https://github.com/miottonicola) contributed the integration
of the grammar into this repository.

This integration builds on the work of OpenGQL and its contributors.

## Examples

The `examples/` directory contains GQL input files for parser testing.

Each `.gql` file should contain a standalone input accepted by the
`gqlProgram` entry point.

These examples demonstrate syntax. They do not provide a database fixture
or verify query execution results.

## Related Projects

- [gql-rs](https://github.com/ntur-lab/gql-rs):
  A Rust project providing an ANTLR-based GQL parser, a typed abstract
  syntax tree, and semantic analysis.

## References

- [ISO/IEC 39075:2024 — Graph Query Language](https://www.iso.org/standard/76120.html)
- [OpenGQL grammar project](https://github.com/opengql/grammar)
- [ANTLR](https://www.antlr.org/)