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


| Refactoring | Rule |
| ---| --- |
| - | var ::= Name &vert; prefixexp '[' exp ']' &vert; prefixexp '.' Name |
| left factor prefixexp | var ::=  Name &vert; prefixexp ( '[' exp ']' &vert; '.' Name ) |
| | |
| - | prefixexp ::= var &vert; functioncall &vert; '(' exp ')' |
| add parentheses | prefixexp ::= ( var ) &vert; functioncall &vert; '(' exp ')' |
| unfold var | prefixexp ::= ( Name &vert; prefixexp ( '[' exp ']' &vert; '.' Name ) ) &vert; functioncall &vert; '(' exp ')' |
| ungroup | prefixexp ::= Name &vert; prefixexp ( '[' exp ']' &vert; '.' Name ) &vert; functioncall &vert; '(' exp ')' |
| reorder alts | prefixexp ::= prefixexp ( '[' exp ']' &vert; '.' Name ) &vert; Name &vert; functioncall &vert; '(' exp ')' |
| remove immediate left recursion | prefixexp ::= ( Name &vert; functioncall &vert; '(' exp ')' ) ( '[' exp ']' &vert; '.' Name )* |
| | |
| - | functioncall ::=  prefixexp args &vert; prefixexp ':' Name args |
| left factor | functioncall ::= prefixexp ( args &vert; ':' Name args ) |
| add parentheses | functioncall ::= ( prefixexp ) ( args &vert; ':' Name args ) |
| unfold prefixexp | functioncall ::= ( ( Name &vert; functioncall &vert; '(' exp ')' ) ( '[' exp ']' &vert; '.' Name )* ) ( args &vert; ':' Name args ) |
| reorder | functioncall ::= ( ( functioncall &vert; Name &vert; '(' exp ')' ) ( '[' exp ']' &vert; '.' Name )* ) ( args &vert; ':' Name args ) |
| group | functioncall ::= ( ( functioncall &vert; ( Name  &vert; '(' exp ')' ) ) ( '[' exp ']' &vert; '.' Name )* ) ( args &vert; ':' Name args ) |
| distribute | functioncall ::= ( functioncall ( '[' exp ']' &vert; '.' Name )* &vert; ( Name  &vert; '(' exp ')' ) ( '[' exp ']' &vert; '.' Name )* ) ( args &vert; ':' Name args ) |
| distribute | functioncall ::= functioncall ( '[' exp ']' &vert; '.' Name )* ( args &vert; ':' Name args ) &vert; ( Name  &vert; '(' exp ')' ) ( '[' exp ']' &vert; '.' Name )* ( args &vert; ':' Name args ) |
| parentheses | functioncall ::= functioncall ( ( '[' exp ']' &vert; '.' Name )* ( args &vert; ':' Name args ) ) &vert; ( ( Name  &vert; '(' exp ')' ) ( '[' exp ']' &vert; '.' Name )* ( args &vert; ':' Name args ) ) |
| remove left recursion | functioncall ::= ( ( Name  &vert; '(' exp ')' ) ( '[' exp ']' &vert; '.' Name )* ( args &vert; ':' Name args ) ) ( ( '[' exp ']' &vert; '.' Name )* ( args &vert; ':' Name args ) )* |

