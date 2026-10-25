# CoC Grammar

An ANTLR 4 grammar for a concrete syntax of the
[Calculus of Constructions (CoC)](https://en.wikipedia.org/wiki/Calculus_of_constructions),
a dependently typed lambda calculus used as a foundation for proof assistants.

Each input contains one term, with these constructs:

- Sorts: `Type` and `Prop`.
- Variables: identifiers such as `x`, `A`, or `proof_1`.
- Application: `f x y`, grouped as `(f x) y`.
- Typed abstraction: `lambda x : A . body` or `λ x : A . body`.
- Dependent product: `forall x : A . body` or `∀ x : A . body`.
  The original grammar's `Ɐ` spelling is also accepted.
- Parenthesized terms: `(lambda x : A . x) y`.

Application binds more tightly than abstraction or dependent products. A binder
extends through the following term; use parentheses when a binder is an
application argument. Colons separate bound variables from their types.

This is a syntax parser, not a type checker or evaluator. The ten UTF-8 files in
[`examples/`](examples/) include sorts, free variables, applications, identity and
constant functions, dependent products, and Unicode binder notation. Free
variables are permitted; parsing does not establish that a term is well typed.

The entry point is `file_` and requires complete input consumption. Run all
examples from the repository root with `mvn -pl coc clean test`, or use
`mvn clean test` in this directory. `desc.xml` configures the same entry point
for Trash Toolkit drivers.
