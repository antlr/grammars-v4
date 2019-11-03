# Golang Grammar

An ANTLR4 grammar for Golang based on [The Go Programming Language Specification](https://golang.org/ref/spec).

## How to use

* Generate lexer and parser with ANTLR4 generator
* Include both generated and base classes to project from corresponding
  directory (C#, Java, or Go). For Go runtime you should add prefix `p.` to
  all semantic predicates, i.e: `lineTerminatorAhead` -> `p.lineTerminatorAhead()`
  and so on.

## Main contributors

* Sasa Coh, Michał Błotniak, 2017
    * Initial version
* Ivan Kochurkin and @fred1268, kvanttt@gmail.com, Positive Technologies, 2019:
    * Separated lexer and parser
    * Fixes and refactoring
* Dmitry Rassadin, flipparassa@gmail.com, Positive Technologies, 2019:
    * Samples set
    * Fixes and refactoring

## License

[BSD-3](https://opensource.org/licenses/BSD-3-Clause)