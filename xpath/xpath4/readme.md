# XPath4 Grammar

## Reference
* [pldb](http://pldb.info/concepts/xpath)
* https://qt4cg.org/specifications/xquery-40/xpath-40.html#id-grammar
* https://qt4cg.org/specifications/grammar-explorer/xpath40/index.html#XPath

## Grammar Notes

### `postfixexpr` — flat form, not left-recursive

The XPath 4.0 spec describes `PostfixExpr` with left-recursive alternatives
(filter, dynamic function call, lookup, method call). Although ANTLR4 supports
direct left recursion, using it here breaks the `IsFuncCall()` semantic
predicate.

The predicate guards `functioncall` inside `primaryexpr`, which in turn is
nested inside `postfixexpr`. When `postfixexpr` is written left-recursively,
ANTLR4's internal rewriting of that rule makes the predicate invisible to the
ALL(\*) prediction for `stepexpr`. As a result, the prediction resolves the
`stepexpr` ambiguity in favour of `postfixexpr` before the predicate is ever
evaluated. When execution then reaches the predicate for tokens like `text`,
`comment`, `node`, or `processing-instruction`, it fires `false` and the parser
reports "no viable alternative" — even though the correct parse is through
`axisstep`.

The fix is to keep `postfixexpr` in the flat, non-left-recursive form used by
the XPath 3.1 grammar:

```
postfixexpr
    : primaryexpr (predicate | positionalargumentlist | lookup
                  | (METHOD_ARROW QName positionalargumentlist))*
    ;
```

This keeps the predicate visible during prediction and allows `text()`,
`comment()`, `node()`, and `processing-instruction()` to be correctly routed
through `axisstep`.

### `choiceitemtype` — also covers parenthesized single types

The spec's `ChoiceItemType` production uses `|` to express a union of two or
more item types, e.g. `(xs:string | xs:integer)`. However, XPath 4.0 also
allows a bare occurrence indicator to be applied to a complex item type by
wrapping it in parentheses, e.g. `(function(node()) as xs:string)*`.

A naïve implementation requiring at least one `|` (the `+` quantifier) rejects
the single-type parenthesised form. The rule is therefore written with `*`
instead:

```
choiceitemtype
    : OP itemtype (P itemtype)* CP
    ;
```

This covers both the single-item parenthesised form `(T)` and the true choice
form `(T1 | T2 | ...)`.

### `NCName` tokens are never produced — use `QName` throughout the parser

The lexer defines `QName : FragQName` where `FragQName` expands to both prefixed
(`prefix:local`) and unprefixed (`local`) name forms. Because `QName` is declared
before `NCName : FragmentNCName`, and both rules match unprefixed names with the
same number of characters, ANTLR4's first-rule-wins tie-breaking means the `NCName`
token is never produced — every unqualified name becomes a `QName` token.

Any parser rule that references the `NCName` token literal will therefore never
match. All such references have been replaced with `QName`:

| Rule | Context |
|---|---|
| `namespacedecl` | namespace prefix in `declare namespace prefix = uri` |
| `postfixexpr` | method name after `=?>` |
| `keyspecifier` | unquoted map-lookup key |
| `markedncname` | name after `#` in namespace/PI constructors |
| `processinginstructionnodetype` | PI name in `processing-instruction(name)` |
| `jnodetype` | JSON node kind selector |
| `fieldname` | field name in `record(field as T)` |
| `wildcard` | namespace wildcards `prefix:*` and `*:local` |

### `URIQualifiedName` with non-empty URI is not tokenised by the lexer

`URIQualifiedName` is defined as `'Q' '{' [^{}]* '}' NCName`. The empty-URI
form `Q{}local` tokenises correctly and is used in the test suite (e.g.
`17 cast as Q{}apple`). However, the non-empty-URI form `Q{uri}local` does
not: once the `[^{}]*` fragment matches one or more characters, ANTLR4's
lexer DFA fails to continue into the trailing `NCName`, committing instead to
the shorter `QName` match for the single character `Q`. The same limitation
exists in the XPath 3.1 grammar. Non-empty `Q{uri}local` patterns and
`Q{uri}*` wildcard expressions are therefore not included in the test suite.