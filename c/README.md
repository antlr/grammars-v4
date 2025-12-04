# C Grammar (ANTLR v4)

The C grammar (`c/C.g4`) supports selecting the C language standard, enabling one grammar to parse multiple C dialects.

- Supported standards: K&R (1978), C90/C89, C99, C11, C17, C23
- Default standard: C11
- Version helpers are defined in both parser and lexer members
  - Parser members: `c/C.g4:35`
  - Lexer members: `c/C.g4:44`

## Using Standard Selection (Java target)

```java
CLexer lexer = new CLexer(input);
lexer.setStandard(CLexer.Standard.C99); // KANDR, C90, C99, C11, C17, C23

CommonTokenStream tokens = new CommonTokenStream(lexer);
CParser parser = new CParser(tokens);
parser.setStandard(CParser.Standard.C99);

CParser.CompilationUnitContext cu = parser.compilationUnit();
```

Set the same standard on both lexer and parser. When a feature is disabled for the selected standard, the corresponding keyword lexes as `Identifier` and grammar constructs using that keyword will not match.

## Gated Features

- C99
  - `inline`: `c/C.g4:546`
  - `restrict`: `c/C.g4:550`
  - `_Bool`, `_Complex`, `_Imaginary`: `c/C.g4:567–570`
- C11
  - `_Atomic`, `_Thread_local`, `_Static_assert`, `_Generic`, `_Noreturn`, `_Alignas`, `_Alignof`: `c/C.g4:564–569`, `c/C.g4:571–573`
- C23
  - `typeof` and `typeof_unqual` tokens: `c/C.g4:574–575`
  - `typeof` in `typeSpecifier`: `c/C.g4:245–246`
  - `u8` string prefix gated in `StringLiteral`: `c/C.g4:801–803`

## Notes

- The grammar historically targeted C11; the default remains C11.
- GCC/Clang and MSVC extensions present in the grammar continue to be recognized; gating focuses on the ISO C standard surface.
- `//` line comments are currently accepted; if you require strict pre-C99 behavior (no `//`), this can be gated similarly.

## Files

- Grammar: `c/C.g4`
- Examples and tests: none in this folder; see repository root for general instructions.
