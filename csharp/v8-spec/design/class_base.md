# Disambiguation of the `class_base` rule

## Context

`class_base` is the colon-prefixed base-type clause of a class declaration:

```
class_declaration → class_base
```

It lists the optional base class and the implemented interfaces.

## The rule

```antlr
class_base
    : {this.IsClassBaseInterfaceList()}? ':' interface_type_list
    | {this.IsClassBaseClassType()}?     ':' class_type
    | ':' class_type ',' interface_type_list
    ;

interface_type_list
    : interface_type (',' interface_type)*
    ;

interface_type : type_name ;
class_type     : type_name | 'object' | 'string' ;
```

The three alternatives correspond to the three legal forms of a C# base-type
clause:

| Alternative | Meaning | Example |
|---|---|---|
| `':' interface_type_list` | implements interfaces only, no explicit base class | `class Foo : IBar, IBaz` |
| `':' class_type` | extends a class, no interfaces | `class Foo : Bar` |
| `':' class_type ',' interface_type_list` | extends a class and implements interfaces | `class Foo : Bar, IBar, IBaz` |

## The problem

All three alternatives begin with the token `':'`.  After the colon, all three
continue with a `type_name` — which reduces to an identifier.  ANTLR4 therefore
sees the same token prefix `':' IDENTIFIER` for alts 1, 2, and 3 and cannot
choose among them by token lookahead alone.

The specific ambiguities are:

### Single type after the colon (e.g., `: Foo`)

Both alt 1 and alt 2 match `':' type_name`.  Without the symbol table, ANTLR4
cannot tell whether `Foo` is an interface (→ alt 1) or a class (→ alt 2).
Without predicates it would always choose alt 1 (`interface_type_list`),
misclassifying every single-base-class declaration as an interface list.

### Multiple types after the colon (e.g., `: Foo, Bar`)

Both alt 1 (`interface_type_list = Foo, Bar`) and alt 3
(`class_type = Foo ',' interface_type_list = Bar`) match `':' type_name ','
type_name`.  Without the symbol table, ANTLR4 cannot tell whether the first
type is a class (→ alt 3) or an interface (→ alt 1).  Without predicates it
would always choose alt 1, misclassifying every class-plus-interfaces base
clause.

### Alt 2 vs alt 3 — no predicate needed

The distinction between alt 2 (`: class_type`, no comma) and alt 3
(`: class_type ',' interface_type_list`, comma present) is resolvable by
LL(*) lookahead: after parsing `class_type`, ANTLR4 looks for `,` to decide
whether to take alt 3 or stop at alt 2.  No predicate is needed for this choice.

## The solution: semantic predicates on alts 1 and 2

Both predicates delegate to the same private helper `ClassBaseTypeCheck`,
which inspects **LT(2)** — the first type name after the `':'` — against the
symbol table.

```csharp
private bool ClassBaseTypeCheck(bool wantInterface)
{
    IToken t = ((CommonTokenStream)InputStream).LT(2);  // LT(1)=':' LT(2)=type name
    if (t == null) return !wantInterface;
    switch (t.Type)
    {
        case CSharpLexer.KW_OBJECT:
        case CSharpLexer.KW_STRING:
            return !wantInterface;  // always class types
    }
    CSharpSymbol sym = SymTable.CurrentScope.LookupChain(t.Text);
    if (sym == null) return !wantInterface;  // unknown → default to class
    if (sym.Kind != CSharpSymbolKind.Type) return !wantInterface;
    CSharpTypeSymbol ts = sym as CSharpTypeSymbol;
    if (ts == null) return !wantInterface;
    bool isInterface = ts.TypeKind == CSharpTypeKind.Interface;
    return wantInterface ? isInterface : !isInterface;
}

public bool IsClassBaseInterfaceList() => ClassBaseTypeCheck(true);
public bool IsClassBaseClassType()     => ClassBaseTypeCheck(false);
```

### `IsClassBaseInterfaceList()`

Returns `true` when LT(2) is positively registered in the symbol table as an
`Interface`.  Fires for alt 1 (`':' interface_type_list`).

### `IsClassBaseClassType()`

Returns `true` when LT(2) is **not** a known interface:

* The keywords `object` and `string` (always class types).
* Any **unknown** identifier (open-world: assume class).
* Any identifier registered as a class, struct, enum, or delegate.

Fires for alt 2 (`':' class_type`), and transitively allows alt 3 to be
selected by ANTLR4's LL(*) engine when a `,` follows `class_type`.

## Predicate evaluation order

ANTLR4 evaluates predicates in declaration order:

1. `IsClassBaseInterfaceList()` — true only for a registered interface as the
   first base type.  If true, alt 1 is chosen (all base types are interfaces).
2. `IsClassBaseClassType()` — true for anything that is not a registered
   interface.  If true, alt 2 or alt 3 is chosen; LL(*) then looks for `','`
   to pick between them.
3. Alt 3 — no predicate; reached when `IsClassBaseClassType()` is true and a
   `,` follows `class_type` in the token stream.

The two predicates are mutually exclusive: for any given LT(2), exactly one
returns `true`.

## Decision table

| LT(2) | `IsClassBaseInterfaceList` | `IsClassBaseClassType` | Chosen alternative |
|---|---|---|---|
| `object` or `string` keyword | false | true | alt 2 or 3 |
| unknown identifier | false | true | alt 2 or 3 |
| registered as Interface | true | false | alt 1 |
| registered as Class / Struct / Enum / Delegate | false | true | alt 2 or 3 |

Within "alt 2 or 3", the `,` after `class_type` selects alt 3; its absence
selects alt 2.

## Open-world limitation

Accurate alt 1 classification requires interface declarations to be registered
in the symbol table before the class that implements them is parsed.  In the
grammar test harness the declarations are **not** pre-registered, so all
user-defined names default to `IsClassBaseClassType() = true` and parse as
alt 2 or alt 3.

## Example parse-tree paths

| C# declaration | `class_base` alternative |
|---|---|
| `class Foo : Bar` (Bar registered as Class) | alt 2 — `':' class_type` |
| `class Foo : IBar` (IBar registered as Interface) | alt 1 — `':' interface_type_list` |
| `class Foo : IBar, IBaz` (both registered as Interface) | alt 1 — `':' interface_type_list` |
| `class Foo : Bar, IBar` (Bar registered as Class) | alt 3 — `':' class_type ',' interface_type_list` |
| `class Foo : Bar` (Bar unregistered) | alt 2 — `':' class_type` (open-world fallback) |
| `class Foo : IBar` (IBar unregistered) | alt 2 — `':' class_type` (open-world fallback) |
| `class Foo : IBar, IBaz` (both unregistered) | alt 3 — `':' class_type ',' interface_type_list` (IBar treated as class) |

## Building and running the example

```sh
cd csharp/v8-spec/design
dotnet build class_base_alternatives.csproj
dotnet run   --project class_base_alternatives.csproj
```

To verify parse trees from the `Generated-CSharp` sandbox:

```sh
cd csharp/v8-spec/Generated-CSharp
./bin/Debug/net10.0/Test.exe -tree ../design/class_base_alternatives.cs
```

Search the output for `class_base` nodes and compare them against the inline
comments in the source file.
