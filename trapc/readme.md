# TrapC Grammar

## Author
Ken Domino

## Copyright
Copyright © 2025 Ken Domino. All rights reserved.

## Reference

https://www.open-std.org/jtc1/sc22/wg14/www/docs/n3423.pdf
https://www.reddit.com/r/C_Programming/comments/1lwd4bz/trapc_memory_safe_c_programming_with_no_ub/

## License
[MIT](https://opensource.org/license/mit)

## Description
This grammar is for TrapC, a memory-safe C programming language.
It makes reference to the [c grammar](https://github.com/antlr/grammars-v4/tree/master/c) in this repo, and uses
cross-grammar Antlr4 import.

Antlr4 overrides in a counterintuitive
manner: rules that are defined first win. The TrapCParser grammar
imports first OverridesParser, which first defines `structOrUnion`, `jumpStatement`, `statement`, and `trapStatement`.
Then it imports CParser from the [c grammar](https://github.com/antlr/grammars-v4/tree/master/c).
While it too defines most of these parser rules, they are ignored because OverridesParser defined
them first.

In addition, a bit of maneuvering is done to use the C grammar base classes in the
TrapC grammar. build.sh and build.ps1 are changed to alter the C grammar port files
for use with TrapC.

*NB: As of April 2026, the project appears to be abandoned.*