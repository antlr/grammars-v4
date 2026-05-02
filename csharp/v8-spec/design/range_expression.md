# `range_expression` — SLL Lookahead Problem and Fix

## The Problem

After fixing `null_coalescing_expression` and `expression`, `trperf` on
`AllInOneNoPreprocessor.cs` reported **max-k = 30** on `range_expression`, again
at the same position in the input.

### Root cause: shared common prefix across alternatives

The original rule was:

```antlr
// Source: §12.10 Range operator
range_expression
    : unary_expression
    | unary_expression? '..' unary_expression?
    ;
```

Alternatives 1 and 2 can both start with `unary_expression`. ANTLR4's SLL
simulation cannot choose between them at the decision point; it must speculatively
follow both alternatives through the ATN until the token stream diverges. The
divergence is the `..` token that follows (alt 2) or does not follow (alt 1)
the `unary_expression`. In the test file the longest `unary_expression` that
precedes `..` spans 30 tokens, giving **max-k = 30**.

---

## The Fix: Left-factoring

Alternative 2 is split on whether a leading `unary_expression` is present, and
the `..` suffix is made optional on alt 1:

```antlr
// Source: §12.10 Range operator
range_expression
    : unary_expression ('..' unary_expression?)?
    | '..' unary_expression?
    ;
```

- **Alt 1** covers: a bare `unary_expression`, and `unary_expression '..'`,
  and `unary_expression '..' unary_expression`.
  After parsing `unary_expression`, only **k = 1** is needed to decide whether
  `..` follows.
- **Alt 2** covers the `..`-prefix form where the left operand is absent
  (`.. unary_expression` or just `..`). This begins with `..`, which is
  syntactically distinct from any token that can start `unary_expression`,
  so it is resolved at LT(1) with no lookahead cost.

The max-k for this decision drops to 1.

---

## Semantic equivalence

The rewritten rule accepts exactly the same language as the original:

| Input form | Original alt | Rewritten form |
|---|---|---|
| `expr` | alt 1 | alt 1, optional suffix absent |
| `expr ..` | alt 2 | alt 1, optional `..` present, right operand absent |
| `expr .. expr` | alt 2 | alt 1, optional `..` present, right operand present |
| `.. expr` | alt 2 | alt 2 |
| `..` | alt 2 | alt 2, operand absent |
