# Standard ML Grammar

An ANTLR 4 grammar for [Standard ML](https://smlfamily.github.io/), a typed
functional programming language with pattern matching and a module system.
Syntax reference: [Standard ML grammar](https://people.mpi-sws.org/~rossberg/sml.html).

The grammar covers core expressions, patterns, declarations, types, datatypes,
exceptions, structures, signatures, and functors. Lexical support includes nested
comments, decimal and hexadecimal integers and words, real literals, strings,
characters, and type variables.

Standard ML permits user-defined infix operators and fixity declarations.
This grammar does not maintain a fixity environment: application and infix
syntax may have multiple parses, and consumers must resolve operator precedence
and associativity using Standard ML's fixity rules. Parsing also does not perform
type checking or module elaboration.

All ten files in [`examples/`](examples/) are tested using the `progs` entry point,
which consumes the entire input. Run `mvn -pl standardml clean test` from the
repository root, or `mvn clean test` from this directory. `desc.xml` configures
the same entry point for Trash Toolkit drivers.
