# Mermaid Grammar

An ANTLR 4 grammar for a subset of [Mermaid](https://mermaid.js.org/), a
text-based diagramming language.

Supported diagram types:

- [Entity relationship diagrams](https://mermaid.js.org/syntax/entityRelationshipDiagram.html):
  entities, relationship cardinalities, identifying and non-identifying links,
  quoted names and labels, attribute blocks, custom types, `PK`/`FK`/`UK` keys,
  and attribute comments.
- [Class diagrams](https://mermaid.js.org/syntax/classDiagram.html):
  class declarations and member blocks, standalone member declarations,
  inheritance, composition, aggregation, dependencies, realization, labels, and
  multiplicities. Class member lines are preserved as text, not type-checked.

Statements and attributes are newline-delimited. `%%` comments are supported.
An input may contain multiple diagrams, and the `mermaid` entry point requires
complete input consumption. Other Mermaid diagram types, directives, front
matter, and styling commands are not currently supported.

All ten files in [`examples/`](examples/) are tested with
`mvn -pl mermaid clean test` from the repository root, or `mvn clean test` from
this directory. `desc.xml` selects the same parser entry point for Trash Toolkit
drivers.
