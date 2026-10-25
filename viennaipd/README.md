# ViennaIPD Grammar

An ANTLR 4 grammar for ViennaIPD, a C-like input control language for scientific
computing. The language provides typed values, expressions, hierarchical
sections, inheritance, and physical quantities.

Reference: [ViennaIPD - An Input Control Language for Scientific Computing](https://www.iue.tuwien.ac.at/pdf/ib_2010/CP2010_Weinbub_1.pdf)
by Josef Weinbub, Karl Rupp, and Siegfried Selberherr.

The grammar supports top-level and nested declarations, optional `ext`, `key`,
and `aux` qualifiers, arithmetic and Boolean expressions, function calls,
scientific notation, strings, arrays, complex-number expressions, quantities,
section references, and multiple and conditional inheritance. It parses syntax
only: it does not evaluate functions, resolve references or inheritance, or check
physical units.

The ten files in [`examples/`](examples/) exercise these constructs. The parser
entry point is `file_` and requires complete input consumption. Run all examples
with `mvn -pl viennaipd clean test` from the repository root, or `mvn clean test`
from this directory. `desc.xml` uses the same entry point for Trash Toolkit
drivers.
