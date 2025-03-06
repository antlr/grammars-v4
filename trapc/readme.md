# TrapC Grammar

## Author
Ken Domino

## Copyright
Copyright © 2025 Ken Domino. All rights reserved.

## License
[MIT](https://opensource.org/license/mit)

## Description
This grammar is for TrapC, a memory-safe C programming language.
It makes reference to the [c grammar](https://github.com/antlr/grammars-v4/tree/a27f0e23507876c4f7babe8d6edd754cd71f5ff9/c) in this repo, and uses
cross-grammar Antlr4 import. NB: Antlr4 overrides in a counterintuitive
manner: rules that are defined first "win." Thus, the TrapC grammar
defines the overriden rules in Overrides.g4. The TrapC.g4 grammar
imports Overrides.g4 first before importing the [c grammar](https://github.com/antlr/grammars-v4/tree/a27f0e23507876c4f7babe8d6edd754cd71f5ff9/c).

