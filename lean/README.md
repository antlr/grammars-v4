# Lean Grammar

An ANTLR4 grammar for a practical subset of the [Lean](https://lean-lang.org/) language,
the functional programming language and interactive theorem prover.

Lean's full surface syntax is extensible through user-defined notation and is
indentation/whitespace sensitive, which is not something a single context-free
ANTLR4 grammar can fully capture. This grammar instead covers the common, fixed
core of the language that appears in everyday Lean 4 source files:

* `import`, `open`, `namespace` / `section` / `mutual` blocks
* `def`, `abbrev`, `theorem`, `lemma`, `example`, `axiom` declarations, including
  equation-style (pattern matching) definitions
* `inductive`, `structure`, `class`, and `instance` declarations, including
  `extends` and `deriving` clauses
* `variable`/`variables`, `set_option`, `attribute`, `#eval`, `#check`, `#print`
  commands
* the common term language: function application, `fun`/`λ` lambdas, `∀`/`∃`
  binders, arrow types, `let`, `have`, `show ... from`, `if ... then ... else`,
  `match ... with`, `do` notation, anonymous constructors (`⟨...⟩`), and the
  usual arithmetic/logical/relational operators
* a generic `by` tactic-block syntax covering tactic sequencing (`;`, `<;>`),
  tactic combinators like `induction ... with`, and nested tactic blocks

Reference: <https://lean-lang.org/>
