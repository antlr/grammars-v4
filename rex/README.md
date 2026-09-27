# REx grammar

ANTLR grammar for the EBNF notation used by the
[REx parser generator](https://github.com/GuntherRademacher/rex-parser-generator).
The parser entry rule is `grammar_` in `rexParser.g4`; `rexLexer.g4` supplies
the token vocabulary. Sample grammars are in `examples/`.

## Target ports

Ports are provided for Antlr4ng, CSharp, Cpp, Dart, Go, Java, JavaScript,
OphiRust, Python3, Rust (antlr4rust), and TypeScript (ANTLR's antlr4 runtime).

The lexer uses the CSharp `LexerBase.Check1()` behavior in each target:
after matching a name character, the next character must not be `:`. This
preserves the existing CSharp predicate exactly. In particular, put whitespace
before `::=`: the predicate is applied after each name character, not just
after a colon. `examples/basic.ebnf` is a minimal parsing smoke test.

The target directories contain the equivalent lexer base classes and, where
needed, `transformGrammar.py`. Run transformations on **copies** of the grammar
in the generated directory, not on the shared source grammar. Go transforms
also handle the generated `parser/` subdirectory. Rust does not implement
lexer superclass inheritance, so its transform inlines the same lookahead
predicate using the generated lexer's `recog` parameter. Transforms are
idempotent, including Rust's before/after-generation invocation.
Dart and Antlr4ng also rename the `context` parser rule to `context_` in
their generated copies, to avoid shadowing the runtime's `Parser.context`
member. The shared grammar and the other targets retain the original name.

OphiRust is distinct from the antlr4rust `Rust` target. Its
`OphiRust/src/lexer_base.rs` supplies the predicate through `SemanticHooks`,
without rewriting the grammar. The generated driver attaches these hooks
when constructing the lexer. If lexer rules or predicates are reordered,
update the `NCNameChar` rule/predicate indices in the hook accordingly.
After building `Generated-OphiRust`, run `cargo test --release` there to check
the hook coordinates and lookahead behavior (colon, ordinary input, and EOF).

## Build and test

With Trash and the selected target toolchain installed, for example:

```sh
dotnet trash gen -t Java
cd Generated-Java
bash build.sh
bash run.sh ../examples/*.ebnf
```

Replace `Java` with another target from the list above. Antlr4ng uses the
antlr4ng TypeScript runtime; it is distinct from the `TypeScript` target.
For the repository's standard test harness, run from this directory:

```powershell
pwsh ../_scripts/test.ps1 -target Java
```
