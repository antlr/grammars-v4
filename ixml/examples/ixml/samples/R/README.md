# R grammar for ixml

This is a grammar for the [R programming language](https://www.r-project.org/).
It is based on a BNF grammar found on [Github](https://github.com/ropensci/ozunconf19/issues/28).

The grammar was made for the presentation "Syntax highlighting for code blocks using iXML" by Pieter Lamers and Nico Verwer,
at [Declarative Amsterdam 2024](https://declarative.amsterdam/program-2024).
A video of the presentation and demo can be found on that page.

The generated parser has been used for syntax coloring.
It can be used in eXist-db by downloading the code found in the [DA2024-syntax-highlighting repository](https://github.com/nverwer/DA2024-syntax-highlighting).
This requires installing a XAR package, available in the [exist-ixml-xar repository](https://github.com/nverwer/exist-ixml-xar).


## Notes

Ambiguity is a problem in the R grammar.
Consider `corpus.df <- data.frame(...)`.
This can be pased as an assignment
```
corpus.df <- ( data.frame(...) )
```
or as a comparison
```
corpus.df < ( - data.frame(...) )
```
In order to prevent this ambiguity, we turn "<-" into a left-pointing arrow in a pre-processing XSLT.
The reverse transformation is done in a post-processing XSLT.
The XSLTs can be found in the [DA2024-syntax-highlighting repository](https://github.com/nverwer/DA2024-syntax-highlighting).

All terminal symbols must be retained by the parser, because we use this grammar for syntax coloring, so no terminals may be lost.
That is why there are non-terminals for almost all terminals.


## Change log

2024-11-26 initial version