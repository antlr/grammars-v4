# Disambiguation of the `type_` rule

## The problem

The C# language specification defines a _type_ as one of:

```
type
    : value_type
    | reference_type
    | type_parameter
    | pointer_type        // unsafe
    ;
```

All three non-pointer alternatives can begin with a bare identifier token.
For example, `Foo` could be:

| Alternative | Example | Why it starts with an identifier |
|---|---|---|
| `type_parameter` | `T` in `Box<T>` | generic type variable |
| `value_type` | `MyStruct`, `MyEnum` | user-defined struct or enum |
| `reference_type` | `MyClass`, `IFoo`, `MyDelegate` | class, interface, or delegate |

`pointer_type` also involves an identifier (e.g., `MyStruct*`), but it is
always syntactically distinguished by the trailing `*`.

Because the three non-pointer alternatives are token-identical at `LT(1)`,
ANTLR4 cannot resolve the choice by lookahead alone.  Without help it would
always pick the first alternative (`type_parameter`), misclassifying every
ordinary type as a generic type variable.

## The solution: semantic predicates

Each ambiguous alternative is guarded by a zero-argument semantic predicate
that inspects `LT(1)` against the symbol table.

```antlr
type_
    : {this.IsTypeParameterName()}? type_parameter
    | {this.IsValueTypeName()}?     value_type
    | {this.IsReferenceTypeName()}? reference_type
    | pointer_type
    ;
```

ANTLR4 evaluates the predicates in order at prediction time.  The first
predicate that returns `true` selects the corresponding alternative; execution
never reaches a later alternative once an earlier one is chosen.

### `IsTypeParameterName()`

Returns `true` when `LT(1)` is present in the current scope chain with kind
`TypeParameter`.

```csharp
CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
return sym != null && sym.Kind == CSharpSymbolKind.TypeParameter;
```

Type parameters are registered as a side effect of parsing
`type_parameter_list` (e.g., the `<T>` in `class Box<T>`), so they are in
scope for the entire class body that follows.

### `IsValueTypeName()`

Returns `true` for two disjoint sets of inputs:

1. **Simple-type keywords** — `bool`, `byte`, `char`, `decimal`, `double`,
   `float`, `int`, `long`, `sbyte`, `short`, `uint`, `ulong`, `ushort` — and
   the tuple opener `(`.  These are decided purely by token type, without a
   symbol-table lookup.
2. **Identifiers registered as struct or enum** — the symbol table is queried;
   the predicate returns `true` only when the symbol's `TypeKind` is `Struct`
   or `Enum`.

### `IsReferenceTypeName()`

The default / open-world predicate.  Returns `true` for:

* The keywords `dynamic`, `object`, `string`.
* `[` — beginning of an `array_type`.
* Any **unknown identifier** (not in the symbol table).
* Any identifier that is not a type parameter and not a struct/enum.

Unknown identifiers are assumed to be reference types because that is the
most common case and matches the open-world assumption: the parser cannot
know every type in the program.

### `pointer_type` — no predicate

Unsafe pointer types (`int*`, `void*`, `char*`) require no predicate because
the `*` suffix is syntactically unambiguous.

## Second-level disambiguation inside `reference_type`

Among reference types that start with an identifier, a further three-way
predicated decision distinguishes `delegate_type`, `interface_type`, and
`class_type`:

```antlr
non_nullable_reference_type
    : {this.IsDelegateTypeName()}?  delegate_type
    | {this.IsInterfaceTypeName()}? interface_type
    | {this.IsClassTypeName()}?     class_type
    | array_type
    | 'dynamic'
    ;
```

`IsDelegateTypeName()` and `IsInterfaceTypeName()` fire only on
**positively-known** symbols (`TypeKind == Delegate` or `Interface`).
`IsClassTypeName()` is the complement default — it returns `true` for
`object`, `string`, unknown identifiers, and anything that is not a delegate
or interface.

## Predicate evaluation order and mutual exclusivity

The three predicates in `type_` are mutually exclusive by construction:

| Predicate | Returns `true` | Returns `false` |
|---|---|---|
| `IsTypeParameterName` | identifier in scope as TypeParameter | everything else |
| `IsValueTypeName` | simple-type keyword, `(`, or identifier as Struct/Enum | everything else |
| `IsReferenceTypeName` | keyword `dynamic`/`object`/`string`, `[`, unknown identifier, or identifier as Class/Interface/Delegate | identifier as TypeParameter or Struct/Enum |

Because ANTLR4 evaluates them in order, the fourth alternative (`pointer_type`)
is reached only when all three predicates return `false` — which in practice
means `LT(1)` is an identifier with a `*` suffix handled further right in the
token stream.

## Open-world limitation

The semantic accuracy of the parse tree depends on the symbol table being
populated before the type reference is parsed.

* **Type parameters** are registered inline while parsing `type_parameter_list`,
  so they are always available within the enclosing generic body.
* **Top-level type declarations** (class, struct, enum, interface, delegate)
  are registered as the parser encounters them in source order.  A forward
  reference to a type declared later in the same file will not yet be in the
  symbol table at the point of use.

When a name is **not** in the symbol table, `IsReferenceTypeName()` returns
`true` (open-world default) and the type is classified as
`reference_type → class_type`.  The tree node is structurally valid — the
parser does not reject the input — but does not reflect the true kind of the
type.  A post-parse binding pass would correct this.

## Example: `type_alternatives.cs`

The file `design/type_alternatives.cs` contains a complete, compilable C#
program that exercises all four `type_` alternatives and has inline comments
showing the exact parse-tree path taken for each type occurrence.

### Building with dotnet

Create a minimal project file alongside the source (or in a sibling directory):

```xml
<!-- type_alternatives.csproj -->
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net10.0</TargetFramework>
    <AllowUnsafeBlocks>true</AllowUnsafeBlocks>
  </PropertyGroup>
</Project>
```

Then build and run:

```sh
cd csharp/v8-spec/design
dotnet build type_alternatives.csproj
dotnet run   --project type_alternatives.csproj
```

Or in one step from any directory:

```sh
dotnet run --project csharp/v8-spec/design/type_alternatives.csproj
```

### Verifying the parse tree

From the `Generated-CSharp` sandbox (after `bash build.sh`):

```sh
./bin/Debug/net10.0/Test.exe -tree ../design/type_alternatives.cs
```

Look for `type_` nodes in the output and compare them against the inline
comments in the source.  The key observations:

* `int`, `bool`, `double`, etc. → `type_ (value_type …)` — keyword path, no
  symbol-table lookup needed.
* `T` inside `Box<T>` → `type_ (type_parameter …)` — registered when `<T>`
  was parsed.
* `Point`, `Color`, `Animal`, `IShape`, `Handler` → `type_ (reference_type
  (non_nullable_reference_type (class_type …)))` — none of these are
  pre-registered in the test harness, so all fall through to the `class_type`
  default.
* `int*`, `char*`, `void*` → `type_ (pointer_type …)` — unsafe, `*` suffix.
* Type arguments (`<int>`, `<string>`) are also `type_` nodes and follow the
  same rules.
