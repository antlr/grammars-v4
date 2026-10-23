# Removal of mutual left-recursion in `primary_expression`

## Background: mutual left-recursion (MLR)

ANTLR4 supports **direct left recursion** within a single rule — it internally
rewrites a rule such as

```antlr
expr : expr '+' expr | atom ;
```

into an iterative (non-recursive) form and produces a correct LL parser.

ANTLR4 does **not** support **mutual left-recursion (MLR)** — a cycle involving
two or more distinct rules, e.g.,

```antlr
A : B 'x' ;   // A calls B as its first token
B : A 'y' ;   // B calls A as its first token
```

When MLR is present, the ANTLR4 tool reports an error and refuses to generate a
parser. The C# language specification contains exactly this pattern in the
`primary_expression` cluster.

---

## The MLR group in the C# specification grammar

The C# spec defines `primary_expression` as a union that includes ten named
sub-rules, each of which begins with `primary_expression` as its first element:

| Sub-rule (spec §) | Left-recursive production |
|---|---|
| `member_access` (§12.8.7) | `primary_expression '.' identifier type_argument_list?` |
| `null_conditional_member_access` (§12.8.8) | `primary_expression '?' '.' identifier type_argument_list? …` |
| `null_forgiving_expression` (§12.8.9) | `primary_expression '!'` |
| `invocation_expression` (§12.8.10) | `primary_expression '(' argument_list? ')'` |
| `null_conditional_element_access` (§12.8.13) | `primary_expression '?' '[' argument_list ']' …` |
| `post_increment_expression` (§12.8.16) | `primary_expression '++'` |
| `post_decrement_expression` (§12.8.16) | `primary_expression '--'` |
| `element_access` (§12.8.12) | `primary_expression '[' argument_list ']'` |
| `pointer_member_access` (§24.6.3, unsafe) | `primary_expression '->' identifier type_argument_list?` |
| `pointer_element_access` (§24.6.4, unsafe) | `primary_expression '[' expression ']'` |

Because `primary_expression` references each of these rules, and each of those
rules references `primary_expression` as its first symbol, ANTLR4 sees an
11-rule MLR cycle and cannot generate a parser.

The grammar preserves the original spec text as a comment block at §12.8.1:

```antlr
// ║ primary_expression
// ║     : literal
// ║     | ...
// ║     | member_access
// ║     | null_conditional_member_access
// ║     | invocation_expression
// ║     | element_access
// ║     | null_conditional_element_access
// ║     | post_increment_expression
// ║     | post_decrement_expression
// ║     | null_forgiving_expression
// ║     | ...
// ║     | pointer_member_access     // unsafe code support
// ║     | pointer_element_access    // unsafe code support
// ║     | stackalloc_expression
// ║     ;
```

---

## The fix: inline all left-recursive alternatives into `primary_expression`

The solution is to replace every call to a left-recursive sub-rule with its
production body placed directly inside `primary_expression`. Because all the
alternatives now live in the same rule, ANTLR4's direct-left-recursion rewriter
can handle the self-references correctly.

The grammar comment records this change:

```
// [CHANGE] This removes a mutual left-recursion group which has been left in the Standard
// [CHANGE] (other uses of MLR have been removed). Without this change the grammar will fail
// [CHANGE] to verify and no sample testing can be done.
```

The resulting `primary_expression` rule (abbreviated):

```antlr
primary_expression
    : literal
    | interpolated_string_expression
    | simple_name
    | tuple_expression
    | parenthesized_expression
    // From: member_access
        | primary_expression '.' identifier type_argument_list?           { this.AsMemberAccess(_localctx); }
        | predefined_type '.' identifier type_argument_list?              { this.AsMemberAccess(_localctx); }
        | qualified_alias_member '.' identifier type_argument_list?       { this.AsMemberAccess(_localctx); }
    // From: null_conditional_member_access
        | primary_expression '?' '.' identifier type_argument_list?
          (null_forgiving_operator? dependent_access)*                    { this.AsNullConditionalMemberAccess(_localctx); }
    // From: invocation_expression
        | primary_expression '(' argument_list? ')'                       { this.AsInvocationExpression(_localctx); }
    // From: element_access and pointer_element_access (unsafe)
        | primary_expression '[' argument_list ']'                        { this.AsElementAccess(_localctx); }
                                                                          { this.ElementAccessSemanticCheck(_localctx); }
    // From: null_conditional_element_access
        | primary_expression '?' '[' argument_list ']'
          (null_forgiving_operator? dependent_access)*                    { this.AsNullConditionalElementAccess(_localctx); }
                                                                          { this.ElementAccessSemanticCheck(_localctx); }
    | this_access
    | base_access
    // From: post_increment_expression
        | primary_expression '++'                                         { this.AsPostIncrementExpression(_localctx); }
    // From: post_decrement_expression
        | primary_expression '--'                                         { this.AsPostDecrementExpression(_localctx); }
    // From: null_forgiving_expression
    // | null_forgiving_expression
        | primary_expression null_forgiving_operator                      { this.AsNullForgivingExpression(_localctx); }
    | array_creation_expression
    | ...
    // From: pointer_member_access (unsafe)
        | primary_expression '->' identifier type_argument_list?          { this.AsPointerMemberAccess(_localctx); }
    // From: pointer_element_access — covered by element_access replacement above
    | stackalloc_expression
    ;
```

Each `As*` action on the inlined alternatives reconstructs the semantic
context that the now-removed named rule would have established, preserving
listener/visitor behaviour.

---

## Rules commented out after the refactoring

Five named rules became unreachable after their sole uses were inlined. Rather
than deleting them, the grammar retains them as commented-out documentation so
that the spec section reference remains visible.

### `null_forgiving_expression` (§12.8.9.1)

```antlr
//null_forgiving_expression
//    : primary_expression null_forgiving_operator
//    ;
```

Inlined into `primary_expression` as `| primary_expression null_forgiving_operator`.

### `invocation_expression` (§12.8.10.1)

```antlr
//invocation_expression
//    : primary_expression '(' argument_list? ')'
//    ;
```

Inlined into `primary_expression` as `| primary_expression '(' argument_list? ')'`.

Consequence for `statement_expression`: the rule previously listed
`invocation_expression` as an alternative.  After inlining, that alternative
became `primary_expression` (which subsumes any invocation):

```antlr
statement_expression
    : null_conditional_invocation_expression
// BUGBUG BUG    | invocation_expression
    | primary_expression
    ...
```

The `BUGBUG` comment records that `primary_expression` is broader than
`invocation_expression` was — it accepts any primary expression where only a
call was valid.  Tightening this would require a semantic predicate or
post-parse validation.

### `element_access` (§12.8.12.1)

```antlr
//element_access
//    : primary_expression '[' argument_list ']'
//    ;
```

Inlined into `primary_expression` as `| primary_expression '[' argument_list ']'`.
The same inlined alternative also replaces `pointer_element_access` (see below).

### `pointer_member_access` (§24.6.3, unsafe)

```antlr
//pointer_member_access
//    : primary_expression '->' identifier type_argument_list?
//    ;
```

Inlined into `primary_expression` as `| primary_expression '->' identifier type_argument_list?`.

### `pointer_element_access` (§24.6.4, unsafe)

```antlr
//pointer_element_access
//    : primary_expression '[' expression ']'
//    ;
```

Covered by the inlined `element_access` alternative (`'[' argument_list ']'`
is a superset of `'[' expression ']'` in practice).

---

## Rules retained as named rules

Five rules in the original MLR group were **not** commented out because they
are still referenced from other parts of the grammar:

| Rule | Retained references |
|---|---|
| `member_access` | `member_declarator` (anonymous-object-creation field initializers) |
| `null_conditional_member_access` | `null_conditional_invocation_expression`, `null_conditional_projection_initializer` |
| `null_conditional_element_access` | `null_conditional_invocation_expression` |
| `post_increment_expression` | `statement_expression` |
| `post_decrement_expression` | `statement_expression` |

These rules still begin with `primary_expression`, but because `primary_expression`
no longer calls them, the MLR cycle is broken.  ANTLR4 encounters them only
when the parent rule (e.g. `member_declarator`) explicitly invokes them, at
which point the parse descends into `primary_expression` without any cycle.

---

## Summary of the refactoring

| Rule | Action | Reason |
|---|---|---|
| `primary_expression` | Expanded in place with all left-recursive alternatives inlined | Converts MLR to direct left recursion, which ANTLR4 supports |
| `null_forgiving_expression` | Commented out | Sole use was in `primary_expression`; now inlined |
| `invocation_expression` | Commented out | Sole use was in `primary_expression`; `statement_expression` updated to use `primary_expression` |
| `element_access` | Commented out | Sole use was in `primary_expression`; now inlined |
| `pointer_member_access` | Commented out | Sole use was in `primary_expression`; now inlined |
| `pointer_element_access` | Commented out | Covered by inlined `element_access` alternative |
| `member_access` | Retained | Still used by `member_declarator` |
| `null_conditional_member_access` | Retained | Still used by `null_conditional_invocation_expression` and `null_conditional_projection_initializer` |
| `null_conditional_element_access` | Retained | Still used by `null_conditional_invocation_expression` |
| `post_increment_expression` | Retained | Still used by `statement_expression` |
| `post_decrement_expression` | Retained | Still used by `statement_expression` |
