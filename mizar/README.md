# Mizar Grammar

An ANTLR4 grammar for [Mizar](http://mizar.org/), the formal mathematics
language and proof-checking system.

## Source
The grammar is transcribed from the official
["Grammar of Mizar"](https://mizar.uwb.edu.pl/language/syntax.html),
version 7.4.01, February 10, 2005, by Grzegorz Bancerek.

Note that `Symbol`, `Identifier`, `Numeral`, and `File-Name` are left as
undefined "system terms" by that reference (their lexical form is resolved
by the vocabulary files a real article imports, not by the grammar itself),
so this grammar gives them pragmatic lexical definitions of its own.
