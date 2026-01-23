# C Language Grammar

This grammar is a refactoring of the EBNF in the ISO/IEC 0900:2024 Specification for the
C language, which contains ambiguity. This grammar tries to correct the ambiguity
with the addition of a symbol table. Since many symbols are declared in include files,
the grammar includes code to call the native C preprocessor to produce a post-processed
file containing all definitions. The grammar implements options to control preprocessor
and semantic predicate functionality, which can be used to explore the ambiguity.

## Ambiguities



## Options

## Reference
* ISO Specification: https://www.open-std.org/jtc1/sc22/wg14/www/projects#9899
* JTC1/SC22/WG14 - C: https://www.open-std.org/jtc1/sc22/wg14/
