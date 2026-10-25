# `expression` — SLL Lookahead Problem and Fix

## The Problem

After fixing `null_coalescing_expression`, `trperf` on `AllInOneNoPreprocessor.cs`
reported **max-k = 30** on the `expression` rule, at the same position in the
input: `list.Select(c => (c.f1, f3: c.f2)).Where(t => t.f2 == 1);`, line 771, column 21.
This is the token length of the longest common prefix between
two alternatives of `expression` in that file.

### Root cause: shared common prefix across alternatives

The original rule was:

```antlr
// Source: §12.24 Expression
expression
    : non_assignment_expression
    | assignment
    ;

assignment
    : unary_expression assignment_operator expression
    ;
```

`non_assignment_expression` expands (via `conditional_expression →
null_coalescing_expression → conditional_or_expression → … → unary_expression`)
to a sequence that can begin with exactly the same tokens as `assignment`.
`assignment` itself begins with `unary_expression`. Since every `unary_expression`
is a valid prefix for both alternatives, ANTLR4's SLL simulation must scan through
the entire `unary_expression` — up to 30 tokens — to reach the distinguishing
`assignment_operator` token (or end of input) before it can select an alternative.

---

## The Fix: Left-factoring

The `assignment` alternative is inlined into `expression` and the
`assignment_operator expression` tail is made optional:

```antlr
// Source: §12.24 Expression
expression
    : non_assignment_expression (assignment_operator expression)?
    ;
```

The parser now unconditionally parses `non_assignment_expression` first. After that
sub-rule returns, only **k = 1** is needed to decide whether an `assignment_operator`
follows. The max-k for this decision drops to 1.

The separate `assignment` rule is no longer referenced by `expression`. It remains
in the grammar because it is referenced elsewhere (e.g. `statement_expression`).

---

## Semantic equivalence and the grammar's permissiveness

The rewritten rule is slightly **more permissive** than the original. The original
`assignment` required the left-hand side to be specifically a `unary_expression`;
the rewritten rule allows any `non_assignment_expression` to serve as the LHS of
an assignment (e.g. `a + b = c` is now grammatically accepted).

This is acceptable: the C# specification already relies on semantic analysis (not
the grammar) to enforce that only certain expression forms are valid assignment
targets (§12.23.1). Invalid LHS forms are caught by the type checker, not the
parser. The same permissiveness is present in many other ANTLR4 C# grammar
implementations.

| Input form | Original alt | Rewritten form |
|---|---|---|
| `expr` (no assignment) | alt 1 (`non_assignment_expression`) | alt 1, optional suffix absent |
| `lhs = rhs` | alt 2 (`assignment`) | alt 1, optional suffix present |
