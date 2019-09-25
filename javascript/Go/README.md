# Go port

**NOTE**: To use the Go version you **MUST** make some tweaks to the grammar files
manually in order to have no compiler errors.

* In the `JavaScriptLexer.g4` file:
    * Go to lines 38, 110, 158-166 and replace the `this` prefix with `p` inside the predicate
    * Go to lines 44, 45 and 174 and replace the `this` prefix with `l` inside the action
* In the `JavaScriptParser.g4` file:
    * Replace all `this.` ocurrences with `p.`
    * Rename the `emptyStatement` parser rule to `emptyStatement_` (Replace all `emptyStatement` ocurrences with `emptyStatement_`)

**WARNING**: After generating both the parser and the lexer, there will be 1 runtime error.

**The runtime error** is caused by a nil pointer dereference, to fix this go to the generated file
`javascript_lexer.go` and go to line 623, then remove the pointer `*` that's preceding
`JavaScriptBaseLexer`.

The cause for this is that ANTLR4 automatically adds a pointer to the embedded `superClass` inside
`[GrammarName]Lexer` (`JavaScriptLexer` in this case) for the Go language. This behaviour is expected 
for single-file grammars (i.e. Both lexer and parser are inside the same file).
But it does not work for multi-file settings where the `superClass` option is set on the lexer grammar.