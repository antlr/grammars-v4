# Protocal Buffers (protobuf) Grammar, Version 3

This is an Antlr4 grammar for protobuf. This grammar
contains contains a symbol table to remove ambiguity
within the grammar.

The parser works in two passes:
1) Parse input and create a symbol table of the data structure
defined in all input. These are encoded in the grammar using actions.
1) Parse the input and resolve ambiguity with the symbol table.
These are encoded in the grammar using semantic predicates.

## Specification

https://protobuf.dev/reference/protobuf/proto3-spec/

