# Glossary

## Divergence point

Given an ambiguous input string with two or more distinct parse trees, a **divergence point**
is a node whose subtree differs across at least two of the parse trees, but whose ancestors —
all the way up to the root — are identical across all parse trees.

### Fixing a divergence point

To eliminate an ambiguity at a divergence point, **every** alternative listed across all
reported parse trees for that decision must receive a semantic predicate — not just the ones
that seem "wrong". If any alternative is left without a predicate, it remains unconditionally
viable and can still be chosen alongside a predicated alternative, leaving the ambiguity
unresolved. The predicates must be collectively mutually exclusive: for any given input token,
exactly one predicate should evaluate to true.
