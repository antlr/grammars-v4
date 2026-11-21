# TLA+ Grammar

An ANTLR 4 grammar for a subset of
[TLA+ (Temporal Logic of Actions)](https://lamport.azurewebsites.net/tla/tla.html)
and its [PlusCal algorithm language](https://lamport.azurewebsites.net/tla/pluscal.html).
TLA+ specifies and reasons about systems, especially concurrent and distributed
systems; PlusCal algorithms can be translated into TLA+ specifications.

The grammar supports module headers, imports, declarations, operator definitions,
assumptions, expressions, and Pascal-style PlusCal algorithms with variables,
processes, assignments, conditionals, loops, and `with` statements. Expressions
include sets, tuples, function calls and indexing, bounded quantifiers, arithmetic,
comparisons, Boolean operators, and basic temporal operators. This is not a
complete TLA+ parser: proof syntax, module instantiation, and C-style PlusCal
algorithms are not supported.

The ten files in [`examples/`](examples/) cover both TLA+ and PlusCal. The original
transfer, tuple, and Hanoi examples are based on
[Learn TLA+](https://learntla.com/).

Run every example from this directory with `mvn clean test`, or from the repository
root with `mvn -pl tlaplus clean test`. The parser entry point is `modules`, which
requires the entire input to be consumed. `desc.xml` configures the same entry
point for Trash Toolkit drivers.
