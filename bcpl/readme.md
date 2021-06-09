# BCPL Grammar

## Source
The source for the grammar is
[Richards, Martin, and Colin Whitby-Strevens. BCPL:
 the language and its compiler. Cambridge University Press, 1981.](https://www.worldcat.org/title/bcpl-the-language-and-its-compiler/oclc/1070255394&referer=brief_results),
Chapter 8. The grammar was entered by hand and the adapted to Antlr4.
Afterwards, the script `transform.sh`, which uses [Trash]() to
remove mutual left recursion, and define an appropriate partition between
lexer and parser.

## Examples
The examples included we[re gathered from the Internet. The best source
is the source for the compiler,
[bcpl.gtz](https://www.cl.cam.ac.uk/~mr10/BCPL.html).

March 14, 2021.
