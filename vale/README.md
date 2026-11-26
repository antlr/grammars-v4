# Vale Grammar

[Vale](https://vale.dev/) is a statically typed, compiled systems programming
language whose standout feature is region-based memory management (single
ownership plus borrow references, and generational/weak references) as a
memory-safe alternative to garbage collection or a borrow checker.

## Source

[GitHub project](https://github.com/ValeLang/Vale)

## Specification

[Language guide](https://vale.dev/guide/introduction) (see also the
`structs`, `interfaces`, `generics`, `references`, `functions`, `patterns`,
`collections`, `modules`, and `regions` guide pages under the same site)

## Notes

Vale uses conventional C-family syntax: braces delimit blocks and statements
end with `;` (no indentation sensitivity, unlike several other grammars in
this project).

The vale.dev guide used to write this grammar documents an archived "0.2
alpha" version of the language and is itself partial: it does not give a
formal lexical grammar, a full keyword list, operator precedence/associativity,
or the complete attribute/modifier syntax, and it marks several features
(simplified interface declarations, `impl` blocks nested inside structs,
full region syntax, match guards) as merely "planned". This grammar fills
those gaps with the common, unambiguous C-family reading — a conventional
left-to-right binary operator precedence ladder — and implements the
constructs that are shown concretely in the guide's examples: `struct`,
`interface`, and `impl` declarations; generics (`<T>`) and region parameters
(`a'`); the ownership/reference type prefixes `&` (borrow), `&&` (weak),
`^` (owning heap allocation) and `?&` (optional borrow); `if`/`else`,
`while`, `foreach`/`in`, `match`, `break`/`continue`, `return`, and the
`set`-based reassignment statement; static (`[#5][...]`) and runtime
(`[](n, fill)`) array literals; and lambdas (`(x) => { ... }`).

Entry rule: `program`
