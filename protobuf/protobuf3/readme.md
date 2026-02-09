# Protocal Buffers (protobuf) Grammar, Version 3

This is an Antlr4 grammar for protobuf. This grammar
contains contains a symbol table to remove ambiguity
within the grammar. There is no disambiguation in protobuf2.

The parser works in two passes through rule `twoPassParse`.
```
twoPassParse : { this.DoRewind(); } proto ;
```
1) In `DoRewind()`, a parse via `proto` is performed to build a symbol table.
The input is rewound to the beginning.
1) In the second pass, `proto()` is called within method `twoPassParse`.
With this parse, the symbol table is utilized to perform correct resolution
of the applied occurrences of identifiers.

## Specification

https://protobuf.dev/reference/protobuf/proto3-spec/

https://protobuf.dev/programming-guides/proto3/

## Notes
Apparently, the Dart port for this grammar cannot be done. This is because the parser
calls for file reading synchronously, whcih cannot be done.

## Reference

