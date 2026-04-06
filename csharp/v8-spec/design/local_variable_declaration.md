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

---

## Comparison with Roslyn's LanguageParser

The Roslyn C# compiler (`LanguageParser.cs`) uses a hand-written recursive-descent
parser and faces the same logical problem, but sidesteps it entirely through
architecture.

### How Roslyn decides "is this a local variable declaration?"

`IsPossibleLocalDeclarationStatement` (line 8501) applies a short, bounded
lookahead:

1. **LT(1) fast path**: `ref`, a declaration modifier (`const`, `static`, …), or
   a predefined type (`int`, `string`, …) → immediately `true`.
2. **`using`/`await using`**: immediately `true`.
3. **`scoped`**: a bounded `IsDefiniteScopedModifier()` scan.
4. **Identifier-headed statements**: `IsPossibleTypedIdentifierStart(LT(1), LT(2))`:
   - LT(2) is another identifier → `true` (pattern `TypeName varName`)
   - LT(2) is `.`, `*`, `?`, `[`, `<`, `::` → `null` (need more)
   - LT(2) is `(` and LT(1) is `var` → `null` (tuple type or deconstruct)
   - Otherwise → `false`
5. **`null` fallback**: `ScanType()` — consumes only the type tokens, then checks
   that what follows is an identifier. The initializer expression is **never
   touched**.

For `var query = 1 + … + 1`:
- LT(1) = `var` (identifier), LT(2) = `query` (identifier).
- `IsPossibleTypedIdentifierStart` returns `true` immediately.
- **Total lookahead: 2 tokens.**

### How Roslyn parses the declaration

`ParseLocalDeclaration` (line 10739) calls `ParseReturnType()` (or `ParseType()`),
which consumes the type tokens — either `var` or an explicit type — and returns
a `TypeSyntax` node. It then calls `ParseVariableDeclarators` to parse the
comma-separated declarator list. Both `var` and an explicit type name go through
the exact same `ParseType()` path; there is no branching on implicit vs explicit.

The binder (not the parser) later decides whether `var` in the resulting
`VariableDeclarationSyntax` is the implicit-typing keyword or a user-defined type
name. The parse tree is identical for both cases.

### Handling of `ref` locals in Roslyn

`ParseType()` (line 7579) checks for a leading `ref` (and `ref readonly`) and
wraps the inner type in a `RefTypeSyntax`. `ParseVariableDeclarator` additionally
checks for `ref` in the initializer (e.g. `ref int x = ref y`). Both paths are
token-distinguishable at LT(1) = `ref`.

### Summary: Roslyn vs v7 vs v8-spec

| Concern | Roslyn | v7 grammar | v8-spec grammar |
|---|---|---|---|
| Decision cost for `var x = long_expr` | 2 tokens (LT(1)+LT(2)) | 1 token (VAR terminal) | O(expr length) — SLL to accept state |
| `var`-as-type-name | Deferred to binder | Not handled (parse always treats `var` as keyword) | Handled at parse time via symbol-table predicate |
| `ref` locals | `ParseType()` sees `ref` at LT(1) | `REF?` prefix on `local_variable_type` | Third predicated alternative (`IsExplicitlyTypedRefLocalVariable`) |
| Implicit/explicit distinction in tree | None — uniform `VariableDeclaration` | None — uniform `local_variable_type` | Full spec-faithful split |
| Parse-time symbol table needed | No | No | Yes |

### Architectural conclusion

Both Roslyn and v7 use the same essential strategy: **unify the declaration form**
and defer the implicit/explicit distinction to a post-parse semantic phase.
This makes the primary parse decision trivially token-based. The v8-spec grammar's
insistence on encoding the spec's implicit/explicit split in the parse tree is the
root cause of the SLL lookahead explosion.

Roslyn's `IsPossibleTypedIdentifierStart` is essentially the hand-written
equivalent of ANTLR4's SLL prediction for `local_variable_type : 'var' | type_`:
it sees two identifiers in a row and immediately commits, never touching the
right-hand side expression.
