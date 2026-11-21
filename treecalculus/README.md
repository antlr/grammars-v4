# Tree Calculus Grammar

Tree calculus is a model of computation, discovered by Barry Jay, with a
single primitive (the triangle, "△") and a single operation, application of
one term to another. Despite this minimal basis, tree calculus is
Turing-complete, and is able to express structures (including self-reflective
programs) that are difficult or impossible to express in the lambda calculus.

The grammar of terms is simply:

```
E ::= △ | E E
```

where parentheses may be used to group sub-terms and override the default
left-associativity of application.

## Reference

* [Tree Calculus](https://treecalcul.us)
