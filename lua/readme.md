# Lua Grammar

## Author

Kazunori Sakamoto, Alexander Alexeev, others

In particular, efforts by Matt Hargett pushed for the refinement of the grammar.

## Specification

[Manual](https://www.lua.org/manual/5.4/manual.html)

## Links

### Source

[parser](https://github.com/lua/lua/blob/6baee9ef9d5657ab582c8a4b9f885ec58ed502d0/lparser.c) (mirror)

[lexer](https://github.com/lua/lua/blob/6baee9ef9d5657ab582c8a4b9f885ec58ed502d0/llex.c) (mirror)

[Github project](https://github.com/lua) (mirror)

### Summary pages

[Wikipedia](https://en.wikipedia.org/wiki/Lua_(programming_language))

[pldb.pub](https://pldb.pub/concepts/lua.html)

### Notes on mutual recursion removal
The EBNF grammar specified in the Lua Manual contains
mutual left recursion. This must be removed in order for
Antlr4 to accept the grammar. The following steps are the
refactorings that were done to bring the grammar.

| ---- | ---- |
| Refactoring | Rule |
| | `var ::= Name | prefixexp '[' exp ']' | prefixexp '.' Name` |
| left factor prefixexp | _`var ::=  Name | prefixexp ( '[' exp ']' | '.' Name )`_ |
| ---- | ---- |
| | `prefixexp ::= var | functioncall | '(' exp ')'` |
| add parentheses | `prefixexp ::= ( var ) | functioncall | '(' exp ')'` |
| unfolding "var" | `prefixexp ::= ( Name | prefixexp ( '[' exp ']' | '.' Name ) ) | functioncall | '(' exp ')'` |
| ungroup | `prefixexp ::= Name | prefixexp ( '[' exp ']' | '.' Name ) | functioncall | '(' exp ')'` |
| reorder alts (for immediate left recursion removal) | `prefixexp ::= prefixexp ( '[' exp ']' | '.' Name ) | Name | functioncall | '(' exp ')'` |
| remove immediate left recursion (please follow Aho, Sethi, Ulman, ISBN 0-201-10088-6, 1988 print, pp47-48, or https://en.wikipedia.org/wiki/Left_recursion | _`prefixexp ::= ( Name | functioncall | '(' exp ')' ) ( '[' exp ']' | '.' Name )*`_ |
| ---- | ---- |
| | `functioncall ::=  prefixexp args | prefixexp ':' Name args` |
| left factor | `functioncall ::= prefixexp ( args | ':' Name args )` |
| add parentheses | `functioncall ::= ( prefixexp ) ( args | ':' Name args )` |
| unfolding prefixexp | _`functioncall ::= ( ( Name | functioncall | '(' exp ')' ) ( '[' exp ']' | '.' Name )* ) ( args | ':' Name args )`_ |
| ---- | ---- |
