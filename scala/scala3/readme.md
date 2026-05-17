# Scala 3

## Source
EBNF adapted from https://docs.scala-lang.org/scala3/reference/syntax.html (read May 6, 2026).
NB: https://scala-lang.org/files/archive/spec/3.4/13-syntax-summary.html seems incomplete (e.g., Import).
So, I decided to not use that, but the "docs" version instead. Note, the "docs" grammar
contains several problems with newlines, semicolons, and statements. I tried to mirror what
the Dotty compiler does rather than assume blind allegience to a human-scraped EBNF.

## Options

The lexer and parser base classes recognise the following command-line option:

| Option | Description |
|--------|-------------|
| `--3.0-migration` | Enable Scala 2-compatible syntax accepted by the Scala 3 compiler under `-source:3.0-migration`. Currently enables: `._` wildcard import selectors (e.g. `import scala.jdk.CollectionConverters._`) and `_` as a wildcard type argument (e.g. `Seq[_]`). Without this flag these constructs are rejected; the Scala 3 equivalents are `.*` and `?` respectively. |

## Reference
* [pldb](http://pldb.info/concepts/scala)
* Dotty compiler parser: https://github.com/scala/scala3/blob/main/compiler/src/dotty/tools/dotc/parsing/Parsers.scala
* Playground: https://onecompiler.com/scala
* Playground: https://www.tutorialspoint.com/compilers/online-scala-compiler.htm

## Parser Rule Coverage

The grammar is tested using the Trash Toolkit `trcover` tool, which instruments the
ANTLR4 parser grammar and tracks which rule call sites are exercised by the example
inputs in `examples/`. Coverage is reported as the number of rule call sites (references
from one parser rule to another) that were reached during parsing.

To regenerate coverage after adding or modifying example files:

```sh
cd Generated-CSharp
dotnet trcover ../examples/*.scala
```

This writes `cover.html`, an annotated copy of the grammar where covered call sites are
highlighted. Call sites with no highlighting were not reached by any example.

### Current coverage

**750 of 763 rule call sites covered (98.3%)**

The 13 uncovered call sites fall on 8 grammar alternative lines, all of which are
permanently unreachable with this grammar and parser.  (Multiple rule references on a
single alternative line each count as a separate call site, hence 13 sites on 8 lines.)

| Grammar location | Reason unreachable |
|---|---|
| `funParamClause` / `typedFunParam` (L165, L169, L173 — 4 call sites) | `simpleType_: LPAREN nameAndType RPAREN` absorbs `(x: Int)` before `funParamClause` is considered in `funTypeArgs`; ANTLR always takes the `infixType` alternative first |
| `INLINE infixExpr matchClause` in `expr1` (L309 — 2 call sites) | `postfixExpr ascription?` (L308) appears earlier and consumes `inline` as a plain identifier; the remaining `x match { … }` is then parsed as a separate statement |
| `LPAREN namedExprInParens … RPAREN` / `namedExprInParens` in `simpleExpr` (L356, L383 — 3 call sites) | `LPAREN exprsInParens RPAREN` appears earlier in `simpleExpr` and always wins; named arguments (`f(x = 1)`) are absorbed by `exprsInParens` via `expr1: id ASSIGN expr` |
| Varargs `LPAREN … postfixExpr Op RPAREN` in `parArgumentExprs` (L390 — 2 call sites) | `LPAREN exprsInParens RPAREN` wins first; `args*` is parsed as a postfix expression inside `exprsInParens` |
| `defSig (COLON type_)?` (abstract declaration) in `defDef` (L704 — 2 call sites) | This alternative **is** executed for abstract method declarations, but the coverage tool cannot track it independently: all three `defDef` alternatives that start with `defSig (COLON type_)?` share the same ATN prefix, so hits are attributed to the first alternative |

### Known grammar limitations

The following are deliberate simplifications that keep the grammar self-contained and
easy to maintain.  Each accepts a slightly broader set of inputs than strict Scala 3
syntax requires.

**`importSelectors` — mixed named/wildcard import lists not supported**

```antlr
importSelectors
    : namedSelector (COMMA importSelectors)?
    | wildCardSelector (COMMA wildCardSelector)*
    ;
```

Valid Scala 3 allows mixing named selectors and wildcards in one import, e.g.
`import foo.{bar, given, *}`.  The rule above only accepts a list of `namedSelector`s
*or* a list of `wildCardSelector`s, not both together.  This covers the common cases
without the added complexity of a fully mixed rule.

**`wildCardSelector`, `negation`, and `variance` — `Op` used for single-character operators**

The lexer has no dedicated tokens for the individual characters `*`, `+`, and `-`;
all contiguous operator characters are emitted as a single `Op` token.  Three grammar
rules therefore use `Op` where only one specific character is valid:

| Rule | Intended operator | Also accepted (over-broadly) |
|---|---|---|
| `wildCardSelector : Op` | `*` import wildcard | any operator sequence |
| `negation : Op` | `-` before a numeric literal | any operator sequence |
| `variance : Op` | `+` or `-` type-parameter variance | any operator sequence |

Adding dedicated single-character lexer tokens (e.g. `STAR`, `MINUS`, `PLUS`) would
require fragmented operator lexing throughout the grammar and is not warranted for a
reference grammar.  The comment on each rule documents the intended restriction.

