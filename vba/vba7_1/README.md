# Visual Basic 7.1 Grammar for ANTLR4

Derived from the Visual Basic 7.1 language reference

https://officeprotocoldoc.z19.web.core.windows.net/files/MS-VBAL/%5bMS-VBAL%5d.pdf

This grammar ignores conditional-compilation statements. The vba_cc grammar can be used against vba files to analyze that portion of the code.

Line endings, whitespace, and comments are traditionally removed from parsers, but the VBA standard dictates when and how some of these are valid, so they remain. Unfortunately, this leaves open room for more inaccuracy. Please report any whitespace bugs.
