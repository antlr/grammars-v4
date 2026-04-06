# Disambiguation of the `non_nullable_reference_type` rule

## Context

`non_nullable_reference_type` is reached via:

```
type_  →  reference_type  →  non_nullable_reference_type
```

It represents every reference type that is not explicitly annotated with `?`.

## The problem

The rule has five alternatives:

```antlr
non_nullable_reference_type
    : {this.IsDelegateTypeName()}?  delegate_type
    | {this.IsInterfaceTypeName()}? interface_type
    | {this.IsClassTypeName()}?     class_type
    | array_type
    | 'dynamic'
    ;
```

The sub-rules of the three predicated alternatives all reduce to `type_name`:

```antlr
delegate_type : type_name ;
interface_type : type_name ;
class_type     : type_name | 'object' | 'string' ;
```

and `type_name` starts with an identifier.  So for any input like `Foo bar`,
ANTLR4 cannot distinguish between:

* `Foo` being the name of a delegate (→ `delegate_type`)
* `Foo` being the name of an interface (→ `interface_type`)
* `Foo` being the name of a class (→ `class_type`)

by token lookahead alone.  Without predicates it would always choose the first
alternative (`delegate_type`), misclassifying every class and interface as a
delegate.

The remaining two alternatives — `array_type` and `'dynamic'` — need no
predicates:

* `'dynamic'` is a reserved keyword token, immediately distinguishable.
* `array_type` expands to `non_array_type rank_specifier+`; the mandatory
  `rank_specifier` (`[` … `]`) following the element type makes it
  syntactically distinct from a bare type name.

## The solution: semantic predicates

Each ambiguous alternative is guarded by a zero-argument semantic predicate
that inspects `LT(1)` against the symbol table.

### `IsDelegateTypeName()`

Returns `true` when `LT(1)` is present in the symbol table as a type with
`TypeKind == Delegate`.

```csharp
CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
if (sym == null || sym.Kind != CSharpSymbolKind.Type) return false;
CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
return ts != null && ts.TypeKind == CSharpTypeKind.Delegate;
```

Returns `false` for anything unknown or not positively declared as a delegate.

### `IsInterfaceTypeName()`

Symmetric with `IsDelegateTypeName()`, but checks `TypeKind == Interface`.

```csharp
return ts != null && ts.TypeKind == CSharpTypeKind.Interface;
```

### `IsClassTypeName()`

The default / open-world predicate.  Returns `true` for:

* The keywords `object` and `string` (always class types).
* Any **unknown identifier** (not in the symbol table) — open-world assumption.
* Any identifier that is not a known interface or delegate.

```csharp
if (t.Type == CSharpLexer.KW_OBJECT || t.Type == CSharpLexer.KW_STRING)
    return true;
CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
if (sym == null) return true;                     // unknown → default to class_type
CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
if (ts == null) return true;
return ts.TypeKind != CSharpTypeKind.Interface
    && ts.TypeKind != CSharpTypeKind.Delegate;
```

## Decision ordering and mutual exclusivity

ANTLR4 evaluates the predicates in declaration order and stops at the first
`true` result.  The three predicates are mutually exclusive for any
**registered** type name:

| Predicate | Returns `true` |
|---|---|
| `IsDelegateTypeName` | identifier registered as Delegate |
| `IsInterfaceTypeName` | identifier registered as Interface |
| `IsClassTypeName` | `object`/`string` keyword; unknown identifier; identifier registered as Class, Struct, or Enum |

For unregistered identifiers, `IsDelegateTypeName` and `IsInterfaceTypeName`
both return `false`, and `IsClassTypeName` returns `true` (open-world default).

## Open-world limitation

Accurate classification of user-defined delegate and interface types requires
those declarations to have been registered in the symbol table before the
type reference is parsed.  When the symbol table has not been populated —
as is the case in the grammar test harness for declarations made in the same
file — all user-defined type names fall through to the `class_type` default.
A post-parse binding pass would correct the classification.

See `non_nullable_reference_type_alternatives.cs` for a demonstration of how
each alternative actually parses in the test harness.

## Example parse-tree paths

| C# type | Parse-tree path |
|---|---|
| `dynamic` | `non_nullable_reference_type: dynamic` |
| `object` | `non_nullable_reference_type → class_type (keyword object)` |
| `string` | `non_nullable_reference_type → class_type (keyword string)` |
| `int[]` | `non_nullable_reference_type → array_type` |
| `string[]` | `non_nullable_reference_type → array_type` |
| `Animal` (registered class) | `non_nullable_reference_type → class_type` |
| `IShape` (registered interface) | `non_nullable_reference_type → interface_type` |
| `Handler` (registered delegate) | `non_nullable_reference_type → delegate_type` |
| `IShape` (unregistered) | `non_nullable_reference_type → class_type` (fallback) |
| `Handler` (unregistered) | `non_nullable_reference_type → class_type` (fallback) |

## Building and running the example

A dedicated project file is provided alongside the source:

```sh
cd csharp/v8-spec/design
dotnet build non_nullable_reference_type_alternatives.csproj
dotnet run   --project non_nullable_reference_type_alternatives.csproj
```

To verify parse trees from the `Generated-CSharp` sandbox:

```sh
cd csharp/v8-spec/Generated-CSharp
./bin/Debug/net10.0/Test.exe -tree ../design/non_nullable_reference_type_alternatives.cs
```

Search the output for `non_nullable_reference_type` nodes and compare them
against the inline comments in the source file.
