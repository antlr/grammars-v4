# `local_variable_declaration` — SLL Lookahead Problem and Refactoring Rationale

## Background

During performance profiling with `trperf`, the rule `local_variable_declaration`
(decision 194 in the generated parser) was found to consume the **entire right-hand
side expression** as lookahead for even the simplest input:

```csharp
// i2.txt
namespace My
{
    public unsafe partial class A : C, I
    {
        [method: Obsolete]
        public A([param: Obsolete] int foo) :
            base(1)
        {
            var query = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1;
        }
    }
}
```

The expression `1 + 1 + ... + 1` (18 ones, 17 plus signs = 35 tokens) plus `var`,
`query`, `=` gives **max-k = 38** — every token up to the semicolon is consumed
before the decision is resolved.

---

## The v8-spec Grammar Structure

The v8-spec grammar faithfully follows the C# language specification §13.6.2,
which separates local variable declarations into three distinct forms:

```antlr
// Source: §13.6.2.1 General
local_variable_declaration
    : {this.IsImplicitlyTypedLocalVariable()}?   implicitly_typed_local_variable_declaration
    | {this.IsExplicitlyTypedLocalVariable()}?   explicitly_typed_local_variable_declaration
    | {this.IsExplicitlyTypedRefLocalVariable()}? explicitly_typed_ref_local_variable_declaration
    ;

// Source: §13.6.2.2 Implicitly typed
implicitly_typed_local_variable_declaration
    : 'var' implicitly_typed_local_variable_declarator
    | ref_kind 'var' ref_local_variable_declarator
    ;

implicitly_typed_local_variable_declarator
    : identifier '=' expression
    ;

// Source: §13.6.2.3 Explicitly typed
explicitly_typed_local_variable_declaration
    : type { this.BeginVariableDeclaration(); }
      explicitly_typed_local_variable_declarators
    ;
```

The predicates are:

```csharp
public bool IsImplicitlyTypedLocalVariable()
{
    IToken t = ((CommonTokenStream)InputStream).LT(1);
    if (t == null) return true;
    if (t.Type != CSharpLexer.KW_VAR) return false;
    if (SymTable.CurrentScope.LookupChain("var") is CSharpSymbol sym
        && sym.Kind == CSharpSymbolKind.Type) return false;
    IToken lt3 = ((CommonTokenStream)InputStream).LT(3);
    if (lt3 == null || lt3.Text != "=") return false;
    IToken lt4 = ((CommonTokenStream)InputStream).LT(4);
    if (lt4 != null && lt4.Text == "{") return false;
    return true;
}

public bool IsExplicitlyTypedLocalVariable()
{
    IToken t = ((CommonTokenStream)InputStream).LT(1);
    if (t == null) return true;
    if (t.Type != CSharpLexer.KW_VAR) return true;
    if (SymTable.CurrentScope.LookupChain("var") is CSharpSymbol sym
        && sym.Kind == CSharpSymbolKind.Type) return true;
    IToken lt3 = ((CommonTokenStream)InputStream).LT(3);
    if (lt3 == null || lt3.Text != "=") return true;
    IToken lt4 = ((CommonTokenStream)InputStream).LT(4);
    if (lt4 != null && lt4.Text == "{") return true;
    return false;
}

public bool IsExplicitlyTypedRefLocalVariable()
{
    IToken t = ((CommonTokenStream)InputStream).LT(1);
    return t != null && t.Type == CSharpLexer.KW_REF;
}
```

These predicates are **correct** and only inspect LT(1)–LT(4). They should
theoretically resolve the decision in at most 4 tokens. Yet max-k = 38.

---

## Why Predicates Don't Help: ANTLR4's SLL Phase

ANTLR4's `AdaptivePredict` runs in two phases. This behaviour is documented
directly in `ParserATNSimulator.cs` (ANTLR4 C# runtime):

> *"SLL collects predicates as it performs closure operations like ANTLR v3 did.
> It **delays predicate evaluation until it reaches an accept state**. This allows
> us to cache the SLL ATN simulation whereas, if we had evaluated predicates
> on-the-fly during closure, the DFA state configuration sets would be different
> and we couldn't build up a suitable DFA."*
> — ParserATNSimulator.cs, lines 126–131

> *"During full LL ATN simulation, closure always evaluates predicates
> **on-the-fly**. This is crucial to reducing the configuration set size during
> closure."*
> — ParserATNSimulator.cs, lines 148–151

### Why on-the-fly SLL predicate evaluation would break DFA caching

The DFA maps `(state, token)` → `next_state` and is shared across all
invocations of the parser. Its correctness depends on the **token sequence
alone** determining the path — not any runtime state.

Predicates depend on runtime state (the symbol table, parser context), not
tokens. Consider:

```antlr
local_variable_declaration
    : {isType("var")}? type explicitly_typed_local_variable_declarators
    | implicitly_typed_local_variable_declaration
    ;
```

Input: `var query = 1`

- **Parse A** (`var` not a type): predicate false → only alt 2 active →
  DFA state after `var` = {alt 2 configs}
- **Parse B** (`var` IS a type): predicate true → both alts active →
  DFA state after `var` = {alt 1 + alt 2 configs}

There is only one DFA slot for the edge `s0 --var-->`, but it would need to
point to different states in A and B. The cache would give wrong answers.
By delaying predicates to the accept state, the DFA intermediate states are
always identical regardless of parse context — only the final accept-state
predicate evaluation varies, and that result is never stored in the DFA.

### The actual flow for `var query = 1 + ... + 1`

1. **SLL phase**: simulates alt 1 and alt 2 (alt 3 is excluded at LT(1)=`var`
   since `ref_kind` requires `ref`). Both alts follow identical ATN paths through
   `expression` — SLL cannot distinguish them token by token.
2. SLL finally reaches the **accept state** after all 38 tokens (end of
   expression).
3. At the accept state, SLL evaluates the predicates:
   `IsImplicit=true`, `IsExplicit=false` → returns alt 1.
4. **LL is never triggered** — SLL succeeded.
5. `SLL_MaxLook = 38`, `LL_MaxLook = 0`.

The `ProfilingATNSimulator` (also in the ANTLR4 C# runtime) tracks these
separately:

```csharp
int SLL_k = sllStopIndex - startIndex + 1;
decisions[decision].SLL_MaxLook = ...;

if (llStopIndex >= 0) {
    int LL_k = llStopIndex - startIndex + 1;
    decisions[decision].LL_MaxLook = ...;
}
```

The `m` (max-k) column from `trperf` reflects `max(SLL_MaxLook, LL_MaxLook)` = 38.

### Why the root cause is the identical token sequences

Alt 1 (implicit): `'var' identifier '=' expression`
Alt 2 (explicit, `var` as type): `type identifier ('=' local_variable_initializer)?`
where `type` can match `var` (because `var` is a `contextual_keyword`, which is
an `identifier`, which feeds into `type` via `namespace_or_type_name`)

For any input of the form `var identifier = non-array-expression`, both
alternatives match **exactly the same token sequence**. SLL has no way to
distinguish them by tokens alone. It must reach the accept state (end of
expression) before it can evaluate the predicates that would resolve the choice.

Adding a predicate to alt 3 (`IsExplicitlyTypedRefLocalVariable`) — which was
tried — had no effect because alt 3 was already excluded at LT(1)=`var`
(syntactically requires `ref`). The 38-token cost was always between alts 1 and 2.

---

## How v6 and v7 Avoid This Problem

The `csharp/v6` and `csharp/v7` grammars use a fundamentally different structure:

```antlr
// v6 and v7
local_variable_declaration
    : (USING | REF | REF READONLY)? local_variable_type local_variable_declarator (
        ',' local_variable_declarator {this.IsLocalVariableDeclaration()}?
    )*
    | FIXED pointer_type fixed_pointer_declarators
    ;

local_variable_type
    : VAR
    | type_
    ;

local_variable_declarator
    : identifier ('=' REF? local_variable_initializer)?
    ;
```

Key differences:

1. **Single unified alternative** for the declaration body. There is no
   top-level choice between implicit and explicit forms. Both are handled by
   the same `local_variable_type local_variable_declarator` sequence.

2. **`local_variable_type : VAR | type_`** as a dedicated rule. `VAR` is the
   first alternative. ANTLR4's SLL sees `VAR` as a terminal token and resolves
   `local_variable_type` at LT(1) — the whole complex of `implicitly_typed_*`
   vs `explicitly_typed_*` rules never arises at prediction time.

3. **No implicit/explicit distinction at parse time.** The grammar does not
   attempt to separate implicit (`var x = expr`) from explicit
   (`SomeType x = expr`) in the parse tree. Both map to the same rule structure.
   The distinction is left to a downstream semantic analysis phase.

4. **`IsLocalVariableDeclaration()` is used only to gate additional declarators
   in a comma list**, not for the primary implicit/explicit decision.

The result: for `var query = 1 + 1 + ... + 1`, the v6/v7 grammar makes its
top-level decision (`VAR` vs other) at **LT(1) = 1 token**. The expression
is never part of the prediction lookahead at all.

The trade-off: v6 and v7 cannot produce a parse tree that distinguishes
`implicitly_typed_local_variable_declaration` from
`explicitly_typed_local_variable_declaration`. For tools that need that
distinction (e.g., refactoring tools that want to know whether a declaration
is implicitly typed), a post-parse tree walk is required.

---

## Summary of the Problem

| Grammar | Structure | max-k for `var x = long-expr` | Handles `var`-as-type-name |
|---|---|---|---|
| v6 | unified `local_variable_type : VAR \| type_` | 1 | No (always treats `var` as implicit) |
| v7 | same as v6 | 1 | No |
| v8-spec | split into implicit/explicit rules + predicates | O(expression length) | Yes |

The v8-spec grammar is **correct** according to the C# specification and
correctly handles the edge case where `var` is declared as a user-defined type
name. However, it pays a significant performance cost: every `var x = expr`
declaration causes ANTLR4's SLL phase to consume the **entire expression**
before evaluating the predicates that could have resolved the decision at LT(4).

---

## The Fundamental Obstacle to a Fix

The core difficulty is that `var identifier = expression` is **syntactically
identical** for both implicit typing (`var` is the keyword) and explicit typing
(`var` is a declared type name). No token-based restructuring of the grammar
can make these alternatives distinguishable earlier, because their token
sequences are truly the same. Only a symbol-table lookup (which the predicates
perform) can distinguish them.

ANTLR4's SLL phase cannot evaluate predicates on-the-fly (as shown above), so
it must follow both alternatives through the full expression ATN before reaching
the accept state where it can apply the predicates.

A refactoring that mirrors the v6/v7 approach — collapsing the three-way
predicated decision into a single `local_variable_type local_variable_declarator`
form — would eliminate the SLL overhead entirely. The cost is losing the
implicit/explicit distinction in the parse tree for the `var` case.
