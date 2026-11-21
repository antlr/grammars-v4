# APL Grammar

An ANTLR4 grammar for [APL](https://en.wikipedia.org/wiki/APL_(programming_language))
(Iverson notation).

References:
- https://en.wikipedia.org/wiki/APL_(programming_language)
- https://softwarepreservation.computerhistory.org/apl/book/APROGRAMMINGLANGUAGE.pdf

## Scope

APL has no operator precedence: every primitive function and operator binds
at the same level, and an expression is evaluated strictly right to left
(e.g. `2+3×4` is `2+(3×4)`). Whether a symbol is applied monadically (one,
right, argument) or dyadically (left and right arguments) depends only on
whether a value precedes it, which this grammar models with

```
expr : operand (function expr)? | function expr ;
```

This grammar supports:
- Assignment (`←`) and branching (`→`), including line labels (`NAME:`).
- The core scalar, mixed, and structural primitive functions (`+ - × ÷ * ⌈
  ⌊ | , ⌹ ○ ⍟ ⌽ ⊖ ⍋ ⍒ ⍎ ⍕ ⍉ ! ≡ ≢ ⍪ ? ∈ ⍷ ⍴ ↑ ↓ ⊥ ⊤ < ≤ = ≥ > ≠ ∨ ∧ ⍱ ⍲ ∼
  ⊣ ⊢ ⊂ ⊃ ⍳`) and the derived-function operators (`/ ⌿ \ ⍀ ¨ ⍣ ⍨`, and
  `∘.` for outer product).
- Numeric literals, including scientific notation, negative numbers
  (written with the high minus `¯`), and complex numbers (`3J4`).
- Character-vector (string) literals, in single quotes.
- Strand (vector) notation, e.g. `1 2 3` or `(1 2)(3 4 5)`.
- Array indexing, e.g. `V[2]` or `M[1;2]`.
- User-defined function definitions (`∇`), with all six niladic/monadic/
  dyadic, with/without explicit result, header forms, and local variables.
- Anonymous "dfn" functions (`{...}`), using `⍺`/`⍵` for the implicit
  arguments.
- Comments (`⍝`) and the quad/quote-quad/system-name (`⎕`, `⍞`, `⎕IO`, ...)
  symbols.

Resolving whether a bare name refers to a variable or a user-defined
function requires a symbol table, which is outside the scope of a
context-free grammar; this grammar resolves the ambiguity by preferring
the dyadic reading (`A FN B`) whenever a value already precedes the name,
and otherwise falls back to the monadic reading (`FN B`).
