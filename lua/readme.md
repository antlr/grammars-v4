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

### Validating test suite files
The Lua files in the examples/lua-test-suite is a collection of source code
from open source projects.
* Test suite for Lua 5.2 (http://www.lua.org/tests/5.2/)
* Test suite for Lua 5.3 http://www.lua.org/tests/lua-5.3.2-tests.tar.gz
* Test suite for Lua 5.4.4: http://www.lua.org/tests/lua-5.4.4-tests.tar.gz
*  Test suite for Selene Lua lint tool v0.20.0: https://github.com/Kampfkarren/selene/tree/0.20.0/selene-lib/tests
* Test suite for full-moon Lua parsing library v0.15.1: https://github.com/Kampfkarren/full-moon/tree/main/full-moon/tests
* Test suite for IntelliJ-Luanalysis IDE plug-in v1.3.0: https://github.com/Benjamin-Dobell/IntelliJ-Luanalysis/tree/v1.3.0/src/test
* Test suite for StyLua formatting tool v.14.1: https://github.com/JohnnyMorganz/StyLua/tree/v0.14.1/tests
* Entire codebase for luvit: https://github.com/luvit/luvit/
* Entire codebase for lit: https://github.com/luvit/lit/
* Entire codebase and test suite for neovim v0.7.2: https://github.com/neovim/neovim/tree/v0.7.2
* Entire codebase for World of Warcraft Interface: https://github.com/tomrus88/BlizzardInterfaceCode
* Benchmarks and conformance test suite for Luau 0.537: https://github.com/Roblox/luau/tree/0.537

Files are classified into those that pass the Lua compiler, and those that pass the Antlr4 grammar
for Lua. Any files that do not pass the Lua compiler are renamed with `.lua.invalid`.
These files are not considered in testing this grammar. Any that passed the compiler but
fail to parse with this grammar are renamed with `.lua.fail`.

To test the sources using the compiler, this Bash code comes in handy.
```
for i in `find . -name '*.lua'`
do
	echo $i
	luac54.exe $i
done
```
