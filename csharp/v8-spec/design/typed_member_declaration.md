# Left-factoring of `typed_member_declaration`

## Context

Field, method, property, and indexer declarations all appear as alternatives
inside the three member-body rules:

```
class_body     → class_member_declaration
struct_body    → struct_member_declaration
interface_body → interface_member_declaration
```

## The original rule (before this change)

```antlr
class_member_declaration
    : constant_declaration
    | field_declaration
    | {this.IsMethodMemberDeclarationAhead()}?  method_declaration
    | {!this.IsMethodMemberDeclarationAhead()}? property_declaration
    | event_declaration
    | {!this.IsMethodMemberDeclarationAhead()}? indexer_declaration
    | operator_declaration
    | constructor_declaration
    | finalizer_declaration
    | static_constructor_declaration
    | type_declaration
    ;
```

`field_declaration`, `method_declaration`, `property_declaration`, and
`indexer_declaration` all begin with the same prefix:

```
attributes? modifiers* type_
```

## The performance problem

For a declaration like

```csharp
protected abstract B<A<T>, A<T>> N() { }
```

`trperf` reported **max-k = 16** for the `class_member_declaration` decision
(Fallback = 0, meaning ANTLR4's SLL mode resolved it without LL fallback).

SLL consumes tokens one by one, simulating all viable alternatives in
parallel.  The 14-token type `B<A<T>, A<T>>` must be consumed before the
member name `N` is visible.  At that point two alternatives are still alive:

| Token 15 | Surviving alternatives |
|---|---|
| `N` | method (member name) **and** explicit-interface indexer (`interface_type '.' 'this'`) |

`indexer_declarator` has the form `type_ interface_type '.' 'this' '[' …]`,
where `interface_type` is a `namespace_or_type_name` that can match any
identifier.  So `N` is a valid `interface_type`, keeping the indexer alive
until token 16 (`(`), where `(` ≠ `.` finally eliminates it.

**Semantic predicates do not help** because SLL treats every `{pred}?` guard
as a free ε-transition (always true).  With Fallback = 0, LL — which
evaluates predicates — never runs.  Adding predicates to method, property,
and indexer leaves k = 16 unchanged.

## Why the sub-rule grouping approach failed

Grouping method + property + indexer into a new `typed_member_declaration`
sub-rule did not force LL fallback either: even with all three alternatives
predicated, SLL could still distinguish them by their terminal tokens (`(`,
`{`, `[`/`.`) at k = 16, so Fallback remained 0 and k stayed at 16.

## The fix: left-factor the shared `type_` prefix

The key insight is that all four declarations share the prefix
`attributes? modifiers* [ref_kind] type_`.  After that prefix is consumed
**once**, the very next token or two uniquely identifies the member kind:

| Next tokens after `type_` | Member kind |
|---|---|
| `this` | simple indexer |
| `identifier (` or `identifier <` | method |
| `identifier {` or `identifier =>` | property |
| `identifier =`, `,`, `;` | field |
| `identifier .` | explicit-interface member (need 1–2 more tokens) |
| `void` | void-returning method |
| `ref_kind type_` | ref-return method / ref property / ref indexer |

The new rule `typed_member_declaration` consumes the shared prefix and then
branches into three _rest_ rules:

```antlr
typed_member_declaration
    : attributes? typed_member_modifier* 'partial'?
      ( ref_kind type_ typed_member_rest_ref
      | type_         typed_member_rest
      | 'void'        typed_member_void_rest
      )
    ;

typed_member_modifier
    : 'new' | 'public' | 'protected' | 'internal' | 'private'
    | 'static' | 'virtual' | 'sealed' | 'override' | 'abstract' | 'extern'
    | 'readonly' | 'volatile' | 'async'
    | unsafe_modifier
    ;

// After ref_kind type_ has been consumed.
typed_member_rest_ref
    : 'this' '[' parameter_list? ']' ref_indexer_body
    | member_name type_parameter_list? '(' parameter_list? ')'
      type_parameter_constraints_clause* ref_method_body
    | member_name ref_property_body
    | interface_type '.' 'this' '[' parameter_list? ']' ref_indexer_body
    ;

// After type_ has been consumed (non-ref, non-void).
typed_member_rest
    : 'this' '[' parameter_list? ']' indexer_body
    | { this.BeginVariableDeclaration(); } variable_declarators ';'
    | member_name type_parameter_list? '(' parameter_list? ')'
      type_parameter_constraints_clause* method_body
    | member_name property_body
    | interface_type '.' 'this' '[' parameter_list? ']' indexer_body
    ;

// After 'void' has been consumed — only a method is valid.
typed_member_void_rest
    : member_name type_parameter_list? '(' parameter_list? ')'
      type_parameter_constraints_clause* method_body
    ;
```

The three parent rules are simplified to:

```antlr
class_member_declaration
    : constant_declaration
    | typed_member_declaration
    | event_declaration
    | operator_declaration
    | constructor_declaration
    | finalizer_declaration
    | static_constructor_declaration
    | type_declaration
    ;
```

(likewise for `struct_member_declaration` and `interface_member_declaration`).

## Why this works

At the `typed_member_rest` decision, all alternatives that start with an
identifier — field, method, property, and explicit-interface indexer — are
**genuinely ambiguous to SLL** (SLL cannot distinguish them at k = 1).  SLL
therefore immediately falls back to LL.  In LL mode, the next 1–2 tokens
after the identifier resolve the choice for every common case:

| k = 2 token | Resolved to |
|---|---|
| `(` | method |
| `<` | generic method (or explicit-interface with generic type — resolved further by LL) |
| `{` | property |
| `=>` | property |
| `=` / `,` / `;` | field |
| `.` `this` | explicit-interface indexer |
| `.` identifier | explicit-interface method / property |

The `'this'` alternative (simple indexer) is unique at k = 1 in SLL, so it
is resolved by SLL without fallback.

The `'void'` and `ref_kind` branches in `typed_member_declaration` are also
quickly identified by SLL at k = 1 (after modifiers are consumed), since
`void` and `ref` are keywords not shared with any other branch.

## Effect on `BeginVariableDeclaration()`

The action `{ this.BeginVariableDeclaration(); }` previously fired inside
`field_declaration`, where `type_` was the last child of the current rule
context.  After left-factoring, `type_` is consumed in `typed_member_declaration`
(the parent), and the action fires at the start of the `typed_member_rest`
field alternative — before any children have been added to `typed_member_rest`.

Fix in `CSharpParserBase.cs`:

```csharp
public void BeginVariableDeclaration()
{
    // After left-factoring, type_ lives in the parent context; fall back
    // to the parent when the current context has no children yet.
    IParseTree ctx = (IParseTree)Context;
    if (ctx.ChildCount == 0 && ctx.Parent != null)
        ctx = (IParseTree)ctx.Parent;
    _pendingVarType = ctx.GetChild(ctx.ChildCount - 1).GetText();
}
```

## Measured result

`trperf` on `x.txt` before and after:

| Rule | max-k (before) | Time (before) | max-k (after) | Time (after) |
|---|---|---|---|---|
| `class_member_declaration` | 16 | 0.53 s | 14 | 0.16 s |
| `typed_member_declaration` / `typed_member_rest` | — | — | not in top | — |

The k = 16 / 0.53 s entry is gone.  `class_member_declaration` now appears at
k = 14 because `field_declaration` (which accepts `static`) stays alive one
token longer than before for the `static B<A<T>, A<T>> O` input; the indexed
`typed_member_rest` LL fallback is not separately attributed in the profile.
