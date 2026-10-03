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
The Rust transform accepts both compact and formatted options blocks and preserves
unrelated options.

OphiRust is distinct from the antlr4rust `Rust` target. Its
`OphiRust/src/lexer_base.rs` supplies the predicate through `SemanticHooks`,
without rewriting the grammar. The generated driver attaches these hooks
when constructing the lexer. If lexer rules or predicates are reordered,
update the `NCNameChar` rule/predicate indices in the hook accordingly.
After building `Generated-OphiRust`, run `cargo test --release` there to check
the hook coordinates, lookahead behavior (colon, ordinary input, and EOF),
and processing-instruction tokenization/mode restoration.

Processing instructions use a target-name mode followed by a content mode.
The `?>` token closes the content mode and restores the previous mode;
question marks in the body are retained without consuming the terminator.
`examples/processing-instructions.ebnf` covers empty and nonempty bodies,
multiline content, inline instructions, and instructions after `<?ENCORE?>`.

`examples/comments-options-unicode.ebnf` covers comments, whitespace directives,
`#x` character classes and equivalences, and delimiter declarations. Whitespace
directives are retained as `option` nodes, not discarded as ordinary comments.
The delimiter operator is two literal backslashes, as in the
[upstream REx grammar](https://github.com/GuntherRademacher/rex-parser-generator/blob/main/rex/src/parser/EbnfParser.ebnf);
REx quoted strings do not interpret backslash escapes.
The Maven tests in `src/test/java/RexReviewTest.java` also assert token boundaries,
option nodes, and rejection of malformed directives and delimiters.

Maven inherits Java base-class source registration from the repository's
parent POM: `Java/` is copied to a generated source root before compilation.
Run `mvn clean test` in this directory to compile from scratch and test the
example corpus.

