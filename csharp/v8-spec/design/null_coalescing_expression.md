# `null_coalescing_expression` — SLL Lookahead Problem and Fix

## The Problem

During performance profiling with `trperf` on `AllInOneNoPreprocessor.cs`, the rule
`null_coalescing_expression` was found to have **max-k = 30**. The 30 reflects the
length (in tokens) of the longest `conditional_or_expression` that preceded a `??`
operator in the test file. For inputs with longer expressions, the max-k would grow
proportionally.

### Root cause: shared common prefix across alternatives

The original rule was:

```antlr
// Source: §12.17 The null coalescing operator
null_coalescing_expression
    : conditional_or_expression
    | conditional_or_expression '??' null_coalescing_expression
    | throw_expression
    ;
```

Alternatives 1 and 2 both begin with `conditional_or_expression`. ANTLR4's SLL
prediction must decide which alternative to enter **before consuming any tokens**.
It does so by speculatively simulating both alternatives in parallel through the ATN
until the token stream diverges. Since the two alternatives are identical up to
the end of `conditional_or_expression`, the simulation must consume every token of
that sub-expression before it reaches the distinguishing `??` token — or the end
of input (confirming alt 1).

In the test file, the longest `conditional_or_expression` before a `??` spans
30 tokens, producing **max-k = 30** for this decision.

---

## The Fix: Left-factoring

The two alternatives that share the common prefix are merged by making the `??`
suffix optional:

```antlr
// Source: §12.17 The null coalescing operator
null_coalescing_expression
    : conditional_or_expression ('??' null_coalescing_expression)?
    | throw_expression
    ;
```

Now the parser commits to consuming `conditional_or_expression` without any
upfront lookahead into its internals. After the sub-rule returns, only **k = 1**
is needed to decide whether `??` follows. The max-k for this decision drops to 1.

Alt 3 (`throw_expression`) begins with the `throw` keyword, which is syntactically
distinct from any token that can start `conditional_or_expression`, so it is
resolved at LT(1) with no lookahead cost.

---

## Semantic equivalence

The rewritten rule accepts exactly the same language as the original:

| Input form | Original alt | Rewritten form |
|---|---|---|
| `expr` | alt 1 | alt 1, optional suffix absent |
| `expr ?? expr` | alt 2 | alt 1, optional suffix present |
| `throw expr` | alt 3 | alt 2 |

No parse tree structure changes beyond the consolidation of the two
`conditional_or_expression`-headed alternatives into one.
