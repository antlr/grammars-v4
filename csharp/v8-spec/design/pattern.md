# Disambiguation of the `pattern` rule

## Context

`pattern` is used in `switch` expressions, `switch` statements, `is` expressions,
and `when` clauses.

```
expression → is_type_expr → pattern
switch_expression → switch_expression_arm → pattern
switch_section → switch_label → pattern
```

## The problem

The grammar defines six alternatives:

```antlr
pattern
    : {this.IsDeclarationPatternAhead()}? declaration_pattern
    | {this.IsConstantPatternAhead()}?    constant_pattern
    | var_pattern
    | positional_pattern
    | property_pattern
    | discard_pattern
    ;

declaration_pattern : type_ simple_designation ;
constant_pattern    : constant_expression ;
var_pattern         : 'var' designation ;
positional_pattern  : type_? '(' subpatterns? ')' property_subpattern? simple_designation? ;
property_pattern    : type_? property_subpattern simple_designation? ;
discard_pattern     : '_' ;
```

The last four alternatives are syntactically distinguishable by token lookahead:

| Alternative | Distinguishing token(s) |
|---|---|
| `var_pattern` | begins with keyword `var` |
| `positional_pattern` | begins with `(` (when `type_?` is absent) or type followed by `(` |
| `property_pattern` | begins with `{` (when `type_?` is absent) or type followed by `{` |
| `discard_pattern` | the single token `_` (standalone, not followed by identifier) |

The first two alternatives, however, share an identical token prefix for many
inputs.  Given `double d`, ANTLR4 cannot tell from lookahead alone whether:

* `double` is the start of a `type_` (→ `declaration_pattern`), or
* `double` is a `constant_expression` (→ `constant_pattern`).

Without guidance, ANTLR4 would always pick the first alternative
(`declaration_pattern`), misclassifying every `constant_pattern` whose
`constant_expression` starts with an identifier or keyword type name.

## The solution: speculative parse

The predicate `IsDeclarationPatternAhead()` performs a **speculative parse**
(analogous to an ANTLR3 syntactic predicate) rather than a simple symbol-table
lookup:

1. Create a temporary `CSharpParser` over the current token stream.
2. Install a `BailErrorStrategy` so any parse failure throws immediately rather
   than attempting recovery.
3. Call `par.type_()` speculatively.
4. After `type_()` returns (or throws), check the very next token: if it is a
   `Simple_Identifier` or the literal `_`, the input matches `type_ simple_designation`
   and this is a `declaration_pattern`.
5. In all cases, rewind the token stream to its original position via
   `InputStream.Seek(savedIndex)`.

```csharp
public bool IsDeclarationPatternAhead()
{
    var par = new CSharpParser((ITokenStream)InputStream);
    par.RemoveErrorListeners();
    par.ErrorHandler = new BailErrorStrategy();
    int savedIndex = InputStream.Index;
    try
    {
        var type = par.type_();
        IToken next = ((CommonTokenStream)InputStream).LT(1);
        return next != null
            && (next.Type == CSharpLexer.Simple_Identifier || next.Text == "_");
    }
    catch
    {
        return false;
    }
    finally
    {
        InputStream.Seek(savedIndex);
    }
}

public bool IsConstantPatternAhead() => !IsDeclarationPatternAhead();
```

The complement `IsConstantPatternAhead()` is simply the negation, making the
two predicates mutually exclusive by construction.

## Why a speculative parse rather than symbol-table lookup

The boundary between `type_` and the start of a `constant_expression` is not
determined solely by the kind of the leading identifier:

* `double d` — `double` is a value-type keyword; `d` is a designation → `declaration_pattern`
* `double.MaxValue` — `double.MaxValue` is a member-access constant expression → `constant_pattern`
* `Shape s` — `Shape` (unregistered) followed by identifier `s` → `declaration_pattern`
* `Shape.Circle` — `Shape.Circle` is a member-access constant expression → `constant_pattern`

A symbol-table predicate alone cannot resolve the `.MaxValue`/`.Circle` vs `d`/`s`
distinction because both start with the same token.  The speculative parse of
`type_()` consumes exactly as many tokens as the grammar would, and the following
token reveals the difference: a simple identifier/discard means `declaration_pattern`;
member-access punctuation (`.`, `(`, `[`) or absence of a following identifier means
`constant_pattern`.

## Decision ordering

ANTLR4 evaluates the predicates in declaration order:

1. `IsDeclarationPatternAhead()` — tries speculative `type_()` parse; if it succeeds and
   is followed by an identifier or `_`, this is a `declaration_pattern`.
2. `IsConstantPatternAhead()` — complement of (1); matches everything else that starts
   like a `constant_expression`.
3. `var_pattern` — no predicate; only reachable when neither (1) nor (2) fired, which
   in practice means the token stream starts with `var`.
4. `positional_pattern` — no predicate; leading `(` or type followed by `(`.
5. `property_pattern` — no predicate; leading `{` or type followed by `{`.
6. `discard_pattern` — no predicate; single `_` token.

## Example parse-tree paths

| C# pattern | Parse-tree path |
|---|---|
| `double d` | `pattern → declaration_pattern` (type_ keyword double + designation d) |
| `double.MaxValue` | `pattern → constant_pattern` (constant_expression: member access) |
| `Shape s` | `pattern → declaration_pattern` (type_ class_type Shape + designation s) |
| `Shape.Circle` | `pattern → constant_pattern` (constant_expression: member access) |
| `42` | `pattern → constant_pattern` (constant_expression: literal) |
| `"hello"` | `pattern → constant_pattern` (constant_expression: literal) |
| `null` | `pattern → constant_pattern` (constant_expression: null literal) |
| `true` | `pattern → constant_pattern` (constant_expression: boolean literal) |
| `var x` | `pattern → var_pattern` |
| `var (a, b)` | `pattern → var_pattern` (tuple designation) |
| `Point(int x, int y)` | `pattern → positional_pattern` |
| `(int x, int y)` | `pattern → positional_pattern` (no type prefix) |
| `{ Name: var n }` | `pattern → property_pattern` |
| `Shape { Area: var a }` | `pattern → property_pattern` (with type prefix) |
| `_` | `pattern → discard_pattern` |

## Building and running the example

```sh
cd csharp/v8-spec/design
dotnet build pattern_alternatives.csproj
dotnet run   --project pattern_alternatives.csproj
```

To verify parse trees from the `Generated-CSharp` sandbox:

```sh
cd csharp/v8-spec/Generated-CSharp
./bin/Debug/net10.0/Test.exe -tree ../design/pattern_alternatives.cs
```

Search the output for `pattern` nodes and compare them against the inline
comments in the source file.
