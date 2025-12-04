![CI](https://github.com/antlr/grammars-v4/actions/workflows/main.yml/badge.svg)

# Grammars-v4

This repository is a collection of formal grammars written for [ANTLR v4](https://github.com/antlr/antlr4)

The root directory name is the all-lowercase name of the language or file format parsed by the grammar. For example, java, cpp, csharp, c, etc...

## FAQ

Please refer to the [grammars-v4 Wiki](https://github.com/antlr/grammars-v4/wiki)

## Code of Conduct

Please refer to [House Rules](https://github.com/antlr/grammars-v4/blob/master/House_Rules.md)

## C Grammar: Standard Version Selection

The C11 grammar in `c/C.g4` now supports selecting the C language standard so the same grammar can parse multiple dialects.

- Supported standards: K&R (1978), C90/C89, C99, C11, C17, C23
- Default: `C11`
- Implementation: version helpers live in the grammar members
  - Parser members: `c/C.g4:35`
  - Lexer members: `c/C.g4:44`

### How to Use (Java target)

- Set the standard on both lexer and parser before parsing.

```java
// input is a CharStream
CLexer lexer = new CLexer(input);
lexer.setStandard(CLexer.Standard.C99); // choose KANDR, C90, C99, C11, C17, C23

CommonTokenStream tokens = new CommonTokenStream(lexer);
CParser parser = new CParser(tokens);
parser.setStandard(CParser.Standard.C99);

CParser.CompilationUnitContext cu = parser.compilationUnit();
```

### What Gets Gated

- C99 keywords and features: `inline`, `restrict`, `_Bool`, `_Complex`, `_Imaginary`
  - Tokens gated at: `c/C.g4:546`, `c/C.g4:550`, `c/C.g4:567–570`
- C11 keywords and features: `_Atomic`, `_Thread_local`, `_Static_assert`, `_Generic`, `_Noreturn`, `_Alignas`, `_Alignof`
  - Tokens gated at: `c/C.g4:564–569`, `c/C.g4:571–573`
- C23 additions: `typeof`, `typeof_unqual`, and `u8` string prefix
  - Type specifier: `c/C.g4:245–246`
  - Tokens: `c/C.g4:574–575`
  - `StringLiteral` gating for `u8`: `c/C.g4:801–803`

When a feature is gated off for an earlier standard, its keyword will lex as an `Identifier` and grammar constructs using that keyword will not match.

### Notes

- The grammar previously targeted C11; the default remains `C11` for compatibility.
- If you require strict pre-C99 behavior (e.g., disallow `//` comments), set the standard to `C90` or `KANDR`. If you need stricter comment gating, see `c/C.g4` and adjust the `LineComment` rule accordingly.
