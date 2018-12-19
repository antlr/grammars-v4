# Go port

**NOTE**: To use the Go version you **MUST** use the JavaScript[Lexer|Parser].g4
files contained in this folder.

**WARNING**: After generating both the parser and the lexer, there will be 2 errors:
1 compile error and 1 runtime error.

**The compile error** is caused by a redeclared function in the generated file `javascript_parser.go`
to fix this simply remove the function named `NewEmptyStatementContext` located in the line 884.

The cause for this is unknown at the time of writing due to the lack of knowledge of the author of
this port about how ANTLR4 generates the files, so it may be an ANTLR4 error instead
of one related to the grammar.

**The runtime error** is caused by a nil pointer dereference, to fix this go to the generated file
`javascript_lexer.go` and go to line 623, then remove the pointer `*` that's preceding
`JavaScriptBaseLexer`.

The cause for this is that ANTLR4 automatically adds a pointer to the embedded `superClass` inside
`[GrammarName]Lexer` (`JavaScriptLexer` in this case) for the Go language. This behaviour is expected 
for single-file grammars (i.e. Both lexer and parser are inside the same file).
But it does not work for multi-file settings where the `superClass` option is set on the lexer grammar.