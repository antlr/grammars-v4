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
                  | (METHOD_ARROW NCName positionalargumentlist))*
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