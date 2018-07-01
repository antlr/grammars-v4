# Molecule Grammar

An ANTLR4 grammar for chemical molecules in [Hill format](https://en.wikipedia.org/wiki/Chemical_formula#Molecular_formula).

Known bug:  The grammar is unable to parse charge, because they are ambiguous, for example `MoO42−`
