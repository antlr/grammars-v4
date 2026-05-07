# Scala 3

## Reference
* [pldb](http://pldb.info/concepts/scala)

## Parser Rule Coverage

The grammar is tested using the Trash Toolkit `trcover` tool, which instruments the
ANTLR4 parser grammar and tracks which rule call sites are exercised by the example
inputs in `examples/`. Coverage is reported as the number of rule call sites (references
from one parser rule to another) that were reached during parsing.

To regenerate coverage after adding or modifying example files:

```sh
cd Generated-CSharp
dotnet trcover ../examples/*.scala
```

This writes `cover.html`, an annotated copy of the grammar where covered call sites are
highlighted. Call sites with no highlighting were not reached by any example.

### Current coverage

**750 of 763 rule call sites covered (98.3%)**

The 8 remaining uncovered alternative lines are all permanently unreachable with this
grammar and parser, for the reasons given below.

| Grammar location | Reason unreachable |
|---|---|
| `funParamClause` / `typedFunParam` (L165, L169, L173) | `simpleType_: LPAREN nameAndType RPAREN` absorbs `(x: Int)` before `funParamClause` is considered in `funTypeArgs`; ANTLR always takes the `infixType` alternative first |
| `INLINE infixExpr matchClause` in `expr1` (L309) | `postfixExpr ascription?` (L308) appears earlier and consumes `inline` as a plain identifier; the remaining `x match { … }` is then parsed as a separate statement |
| `LPAREN namedExprInParens … RPAREN` / `namedExprInParens` in `simpleExpr` (L356, L383) | `LPAREN exprsInParens RPAREN` appears earlier in `simpleExpr` and always wins; named arguments (`f(x = 1)`) are absorbed by `exprsInParens` via `expr1: id ASSIGN expr` |
| Varargs `LPAREN … postfixExpr Op RPAREN` in `parArgumentExprs` (L390) | `LPAREN exprsInParens RPAREN` wins first; `args*` is parsed as a postfix expression inside `exprsInParens` |
| `defSig (COLON type_)?` (abstract declaration) in `defDef` (L704) | This alternative **is** executed for abstract method declarations, but the coverage tool cannot track it independently: all three `defDef` alternatives that start with `defSig (COLON type_)?` share the same ATN prefix, so hits are attributed to the first alternative |
