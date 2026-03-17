## Summary

C# grammar targeting ECMA-334 8th edition (draft), with full support of C# 8 features in the
Roslyn compiler.
This grammar is based on the v7 grammar and adds the new features and enhancements for C#:
- [Readonly members](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/struct#readonly-instance-members)
- [Default interface members](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/interface#default-interface-members)
- [Pattern matching enhancements](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/patterns):
    - [Switch expressions](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/switch-expression)
    - [Property patterns](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/patterns#property-pattern)
    - [Tuple patterns](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/patterns#tuple-pattern)
    - [Positional patterns](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/patterns#positional-pattern)
- [Using declarations](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/statements/using)
- [Static local functions](https://learn.microsoft.com/en-us/dotnet/csharp/programming-guide/classes-and-structs/local-functions)
- [Disposable ref structs](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/ref-struct)
- [Nullable reference types](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/builtin-types/nullable-reference-types)
- [Asynchronous streams](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/statements/iteration-statements#await-foreach)
- [Indices and ranges](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/member-access-operators#range-operator-)
- [Null-coalescing assignment](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/assignment-operator#null-coalescing-assignment)
- [Unmanaged constructed types](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/keywords/where-generic-type-constraint)
- [Stackalloc in nested expressions](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/operators/stackalloc)
- [Enhancement of interpolated verbatim strings](https://learn.microsoft.com/en-us/dotnet/csharp/language-reference/tokens/interpolated)

## Preprocessing

This grammar handles C# preprocessor directives (`#if`, `#elif`, `#else`, `#endif`, `#define`, `#undef`) as part
of normal lexing — no separate preprocessor pass is required. The logic lives entirely in `CSharpLexerBase` via
a `NextToken()` override.

When a `#if` / `#elif` / `#else` condition evaluates to false, the skipped source text is collected into a single
`SKIPPED_SECTION` token emitted on the hidden channel. The parser never sees the false branch. Nested `#if` blocks
are handled correctly by tracking a condition stack and a "was any branch taken" stack.

Supported directives:

| Directive | Behaviour |
|---|---|
| `#define SYM` | Adds `SYM` to the active symbol set (only when in an active section) |
| `#undef SYM` | Removes `SYM` from the active symbol set (only when in an active section) |
| `#if EXPR` | Evaluates `EXPR`; skips the block if false |
| `#elif EXPR` | Evaluates `EXPR` if no prior branch was taken; skips the block if false |
| `#else` | Active if no prior branch was taken |
| `#endif` | Closes the current conditional block |
| `#region` / `#endregion` | Lexed and discarded (no semantic effect) |
| `#line` / `#pragma` / `#warning` / `#error` | Lexed and discarded |

Preprocessor expressions support `!`, `&&`, `||`, `==`, `!=`, parentheses, `true`, `false`, and symbol names.

### Command-Line Options

Symbols can be pre-defined before parsing using the `--D` option (analogous to `csc /define:`):

```
--DSYM              Define a single symbol SYM
--DSYM1;SYM2;SYM3   Define multiple symbols separated by semicolons
```

**Java** (`CSharpLexerBase.java`): pass `--DSYM` as a JVM system property or as a program argument;
the base class reads `System.getProperty("sun.java.command", "")` and scans for `--D` prefixed tokens.

**C#** (`CSharpLexerBase.cs`): pass `--DSYM` on the command line; the base class reads
`Environment.GetCommandLineArgs()` and scans for `--D` prefixed arguments.

Example (C# test harness):

```
Test.exe --DCOMPILERCORE myfile.cs
Test.exe --DDEBUG;TRACE myfile.cs
```

### `--no-semantics`

The parser uses one semantic predicate, `IsLocalVariableDeclaration()`, to disambiguate
`var x = ...` (implicitly-typed local) from a type named `var`. Passing `--no-semantics`
disables this predicate (it returns `true` unconditionally), which lets the parser run
without any context-sensitive logic — useful for quick batch testing or fuzzing.

```
--no-semantics                           Disable all semantic predicates
--no-semantics=IsLocalVariableDeclaration  Disable a specific predicate by name
```

**Java**: pass as a JVM system property: `-Dno-semantics` is not used here; instead pass
`--no-semantics` in `sun.java.command` (i.e. as a normal program argument to the test harness).

**C#**: pass on the command line to the test harness:

```
Test.exe --no-semantics myfile.cs
```

## Parse Tree Editing

The C# standard defines `primary_expression` and several other rules (`member_access`,
`invocation_expression`, `element_access`, etc.) as a **mutually left-recursive group** —
each sub-rule has `primary_expression` as its first element, and `primary_expression` itself
lists those sub-rules as alternatives. ANTLR cannot handle mutual left recursion (only direct
left recursion within a single rule), so the grammar would fail to compile as written in the
standard.

The fix is to **inline** all those sub-rules directly as alternatives of `primary_expression`,
exploiting ANTLR's own direct-left-recursion rewriting. That gives the correct parse, but it
**collapses the parse tree**: where the standard grammar would produce a
`primary_expression` → `member_access` → … subtree, the inlined grammar produces a flat
`primary_expression` node with the member-access children directly underneath it.

The `As*` actions in `CSharpParserBase` restore the missing layer. At the end of each inlined
alternative they wrap the current `primary_expression` node's children inside a freshly-created
context of the correct type (e.g. `Member_accessContext`), then make that new node the sole
child of the `primary_expression` node. The resulting tree is shaped as if the sub-rules had
never been inlined, so external tooling that walks the tree expecting `member_accessContext`,
`invocation_expressionContext`, etc. nodes continues to work without modification.

`ElementAccessSemanticCheck` is separate: it enforces a semantic rule from §12.8.11-2 that
`array_creation_expression[…]` and `stackalloc_expression[…]` must have an initializer on
the left-hand side of an element access — a constraint the grammar alone cannot express.

## Grammar Symbol Mapping

See [grammar-symbol-mapping.md](grammar-symbol-mapping.md) for the full mapping between ANTLR4 rule names
and their corresponding symbols in the ECMA-334 7th edition specification.

## Reference
* [pldb](http://pldb.info/concepts/csharp)
* [ECMA 334](https://ecma-international.org/publications-and-standards/standards/ecma-334/)
* [csharpstandard, Working space for ECMA-TC49-TG2, the C# standard committee](https://github.com/dotnet/csharpstandard).

## Performance

Parse of first 1000 files of `find testing/roslyn/src -name '*.cs'` is 871 +/- 8 tokens per second (SD). Sample size 5, port CSharp. 915535 tokens.

## License

MIT
