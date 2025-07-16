# Protocal Buffers (protobuf) Grammar, Version 3

This is an Antlr4 grammar for protobuf. This grammar
contains contains a symbol table to remove ambiguity
within the grammar.

The parser works in two passes through rule `twoPassParse`, which looks admittedly
a little weird.
```
twoPassParse : proto { this.DoRewind(); } proto ;
```
1) In the first pass, `proto()` parses the full input (it is a proper EOF-terminated
start rule). It creates a symbol table for the input using actions sprinkled in the grammar.
Great care is taken to make sure actions don't "hide" semantic predicates from firing.
1) `DoRewind()` is called to rewind the input and reset the parser for a second parse.
1) In the second pass, `proto()` is called again, this time using the full
symbol table. The predicates in the grammmar select alts to remove ambiguity.

## Specification

https://protobuf.dev/reference/protobuf/proto3-spec/

https://protobuf.dev/programming-guides/proto3/
