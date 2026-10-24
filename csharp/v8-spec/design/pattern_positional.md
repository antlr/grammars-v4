# Disambiguation of `constant_pattern` vs `positional_pattern`

## Context

This document describes a **follow-on fix** to the ambiguity documented in
`pattern.md`.  After `IsDeclarationPatternAhead` / `IsConstantPatternAhead`
were introduced to separate `declaration_pattern` from `constant_pattern`, a
residual ambiguity remained between `constant_pattern` (alt 2) and
`positional_pattern` (alt 4) for inputs that begin with `(`.

## The problem

`positional_pattern` has an optional leading `type_?`:

```antlr
positional_pattern : type_? '(' subpatterns? ')' property_subpattern? simple_designation? ;
```

When the `type_?` is absent, the pattern begins with `(`.  A `constant_pattern`
whose `constant_expression` is a parenthesised expression also begins with `(`:

```csharp
case ("rock", "scissors"):   // positional_pattern — tuple of two string patterns
case ("hello"):              // constant_pattern  — parenthesised string constant
```

With the original `IsConstantPatternAhead() => !IsDeclarationPatternAhead()`,
a `("rock", "scissors")` input was reported as ambiguous between alt 2
(`constant_pattern`) and alt 4 (`positional_pattern`) because:

* `IsDeclarationPatternAhead()` returns `false` (no type + designation follows).
* `IsConstantPatternAhead()` therefore returns `true`.
* `positional_pattern` had no predicate, so it was unconditionally viable.

Both alternatives matched and ANTLR4 flagged the ambiguity (decision 31).

## The solution

### Extending `IsConstantPatternAhead`

A tuple-positional pattern is recognised by a **comma at parenthesis depth 1**.
Tuple expressions cannot be C# compile-time constants (the C# specification,
§12.20, requires a constant expression; tuples are not constants), so any `(`
that contains a top-level comma must be `positional_pattern`, not
`constant_pattern`.

`IsConstantPatternAhead` was extended with a second guard after the
declaration-pattern check:

```csharp
public bool IsConstantPatternAhead()
{
    if (IsDeclarationPatternAhead()) return false;

    // A '(' followed by a comma at depth 1 is a tuple-positional pattern
    // like ("rock", "scissors") or (0, 0).  Tuple expressions are not C#
    // compile-time constants, so this can only match positional_pattern
    // (alt 4), not constant_pattern (alt 2).  Return false to suppress
    // alt 2 and eliminate the ambiguity.
    var ts = (ITokenStream)InputStream;
    if (ts.LT(1).Type == CSharpLexer.TK_LPAREN)
    {
        int depth = 0, i = 1;
        while (true)
        {
            IToken tok = ts.LT(i++);
            if (tok.Type == TokenConstants.EOF) break;
            if (tok.Type == CSharpLexer.TK_LPAREN)       depth++;
            else if (tok.Type == CSharpLexer.TK_RPAREN) { depth--; if (depth == 0) break; }
            else if (tok.Type == CSharpLexer.TK_COMMA && depth == 1) return false;
        }
    }
    return true;
}
```

### Adding `IsPositionalPatternAhead` and gating `positional_pattern`

Because the predicates on alts 1 and 2 are no longer a complete partition of
all inputs, `positional_pattern` must be explicitly gated so that it is
suppressed when `IsConstantPatternAhead` returns `true`.  The complement
predicate and the grammar change together enforce mutual exclusion:

```csharp
public bool IsPositionalPatternAhead() => !IsConstantPatternAhead();
```

```antlr
pattern
    : {this.IsDeclarationPatternAhead()}? declaration_pattern
    | {this.IsConstantPatternAhead()}?    constant_pattern
    | var_pattern
    | {this.IsPositionalPatternAhead()}?  positional_pattern
    | property_pattern
    | discard_pattern
    ;
```

## Why the comma-at-depth-1 test is sufficient

The check is conservative: it only returns `false` when there is definitely a
top-level comma inside the parentheses. Cases where `IsConstantPatternAhead` must
still return `true` for a `(`-headed input:

| Input | Reason |
|---|---|
| `("hello")` | No comma → not suppressed → remains `constant_pattern` |
| `(x + y)` | No comma → not suppressed → remains `constant_pattern` |
| `("rock", "scissors")` | Comma at depth 1 → suppressed → routes to `positional_pattern` |
| `(0, 0)` | Comma at depth 1 → suppressed → routes to `positional_pattern` |
| `((a, b), c)` | Comma at depth 1 (after inner pair closes) → suppressed |

Nested parentheses are handled correctly because the scan tracks `depth` and only
reacts to commas at `depth == 1`.

## Decision ordering (revised)

| Alt | Predicate | Fires when |
|---|---|---|
| 1 `declaration_pattern` | `IsDeclarationPatternAhead()` | Speculative `type_()` succeeds and is followed by `Simple_Identifier` or `_` |
| 2 `constant_pattern` | `IsConstantPatternAhead()` | Not a declaration pattern **and** not a paren-with-comma |
| 3 `var_pattern` | _(none)_ | Reachable only when alts 1 and 2 are blocked; `var` keyword |
| 4 `positional_pattern` | `IsPositionalPatternAhead()` | Complement of alt 2; paren-with-comma or type-headed `(` |
| 5 `property_pattern` | _(none)_ | Leading `{` or type followed by `{` |
| 6 `discard_pattern` | _(none)_ | Single `_` token |

Alts 1 and 2 remain mutually exclusive by construction (negation).
Alts 2 and 4 are now mutually exclusive by construction (negation via
`IsPositionalPatternAhead`).

## Example parse-tree paths

| C# pattern | Predicate outcome | Parse-tree path |
|---|---|---|
| `("hello")` | alt 2 true | `pattern → constant_pattern` (parenthesised string) |
| `("rock", "scissors")` | alt 2 false (comma at depth 1), alt 4 true | `pattern → positional_pattern` |
| `(0, 0)` | alt 2 false (comma at depth 1), alt 4 true | `pattern → positional_pattern` |
| `((a, b), c)` | alt 2 false (comma at depth 1), alt 4 true | `pattern → positional_pattern` |
| `Point(0, 0)` | alt 1 false, alt 2 false (speculative `type_()` followed by `(`), alt 4 true | `pattern → positional_pattern` (type-headed) |
