# C Language Grammar

This grammar is a refactoring of the EBNF in the ISO/IEC 0900:2024 Specification for the
C language, which contains ambiguity. This grammar tries to correct the ambiguity
with the addition of a symbol table. Since many symbols are declared in include files,
the grammar includes code to call the native C preprocessor to produce a post-processed
file containing all definitions. The grammar implements options to control preprocessor
and semantic predicate functionality, which can be used to explore the ambiguity.

## Ambiguities



## Options

The parser and lexer base classes implement the following command-line options:

### Preprocessor Options (CLexerBase)

These options control which C preprocessor is used before parsing:

| Option | Description |
|--------|-------------|
| `--gcc` | Use the GCC preprocessor (`gcc -E`). This is the default if no preprocessor is specified. |
| `--clang` | Use the Clang preprocessor (`clang -E`). |
| `--vsc` | Use the Visual Studio C preprocessor (`cl.exe /E`). Windows only. |
| `--nopp` | Skip preprocessing entirely. Use this if the input is already preprocessed. |

### Parser Options (CParserBase)

These options control semantic predicates and symbol table output:

| Option | Description |
|--------|-------------|
| `--debug` | Enable debug output for semantic predicates and symbol table operations. |
| `--no-semantics` | Disable all semantic predicates. Each predicate returns `true`, effectively disabling symbol table lookups for parsing decisions. |
| `--no-semantics=Func1,Func2,...` | Disable only the specified semantic predicates. Available predicates: `IsAlignmentSpecifier`, `IsAtomicTypeSpecifier`, `IsAttributeDeclaration`, `IsAttributeSpecifier`, `IsAttributeSpecifierSequence`, `IsDeclaration`, `IsDeclarationSpecifier`, `IsTypeSpecifierQualifier`, `IsEnumSpecifier`, `IsFunctionSpecifier`, `IsStatement`, `IsStaticAssertDeclaration`, `IsStorageClassSpecifier`, `IsStructOrUnionSpecifier`, `IsTypedefName`, `IsTypeofSpecifier`, `IsTypeQualifier`, `IsTypeSpecifier`, `IsCast`, `IsNullStructDeclarationListExtension`. |
| `--output-symbol-table` | Output the symbol table to stderr after parsing completes. Shows all non-predefined symbols with their classification and source location. |
| `--output-applied-occurrences` | Output applied occurrences (identifier references) to stderr as they are resolved. Each line shows the applied occurrence location and its corresponding defining occurrence location. |

## Reference
* ISO Specification: https://www.open-std.org/jtc1/sc22/wg14/www/projects#9899
* JTC1/SC22/WG14 - C: https://www.open-std.org/jtc1/sc22/wg14/
