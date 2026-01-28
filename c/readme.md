# C Language Grammar

## Introduction

This is an ANTLR4 grammar for the C programming language, based on the ISO/IEC 9899:2024 specification (C23). The grammar is organized into two files: `CLexer.g4` for lexical analysis and `CParser.g4` for parsing. Parser rules are ordered to correspond with the sections in the ISO C specification.

### Preprocessor Integration

Unlike many C grammars that assume pre-processed input, this grammar integrates directly with native C preprocessors (GCC, Clang, or Visual Studio). This integration is essential because C programs typically rely on header files (`#include`) that define types, macros, and declarations. Without preprocessing, the parser would lack the type information necessary to correctly parse the source code.

The lexer base class (`CLexerBase`) automatically invokes the selected preprocessor before tokenization begins. By default, the grammar uses `gcc -E` for preprocessing, but this can be configured via command-line options (see Options below). The `--nopp` option allows parsing of already-preprocessed files.

### Symbol Table for Disambiguation

The C language grammar contains inherent ambiguities that cannot be resolved through syntax alone. The most significant is the "typedef problem": when the parser encounters an identifier, it cannot determine from context alone whether it represents a type name (from a `typedef`) or a variable/function name. For example:

```c
foo * bar;
```

This could be parsed as either:
- A declaration of `bar` as a pointer to type `foo` (if `foo` is a typedef)
- A multiplication expression of `foo` times `bar` (if `foo` is a variable)

To resolve such ambiguities, this grammar implements a symbol table that tracks type definitions as they are parsed. Semantic predicates in the parser (such as `IsTypedefName`, `IsTypeSpecifier`, and `IsCast`) query the symbol table to make context-sensitive parsing decisions. The symbol table maintains proper scoping, pushing and popping block scopes as compound statements are entered and exited.

### Multi-Target Support

The grammar includes target-specific implementations for multiple languages: Java, TypeScript, C#, Dart, and Antlr4ng. Each target provides its own implementation of `CLexerBase` and `CParserBase` with the preprocessor integration and symbol table logic.

### GNU/GCC Extensions

In addition to standard C23, this grammar supports common GNU/GCC extensions including:
- GNU-style attributes (`__attribute__`)
- Inline assembly (`asm`, `__asm__`)
- Additional type specifiers (`__int128`, `_Float128`, etc.)
- Statement expressions (`({ ... })`)
- Label addresses (`&&label`)
- Various built-in functions (`__builtin_va_arg`, `__builtin_offsetof`, etc.)

Visual C extensions such as calling conventions (`__cdecl`, `__stdcall`, etc.) and `__declspec` are also supported.

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
| `--D<macro>[=<value>]` | Define a preprocessor macro. Passed to the preprocessor as `-D<macro>[=<value>]`. Can be specified multiple times. Example: `--DDEBUG=1` |
| `--I<path>` | Add a directory to the include search path. Passed to the preprocessor as `-I<path>`. Can be specified multiple times. Example: `--I/usr/local/include` |

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
