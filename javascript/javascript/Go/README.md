# Go port

**NOTE**: To use the Go version you **MUST** make some tweaks to the grammar files
manually in order to have no compiler errors.

* In the `JavaScriptLexer.g4` file, replace all occurrences of `this` with:
    * `l` inside actions (i.e., next to `ProcessOpenBrace`, `ProcessCloseBrace`, `ProcessStringLiteral`).
    * `p` inside predicates (i.e., all other).
* In the `JavaScriptParser.g4` file:
    * Replace all occurrences of `this` with `p`.
    * Replace all occurrences of `emptyStatement` parser rule with `emptyStatement_`.

**WARNING**: After generating both the parser and the lexer, there will be 1 runtime error.

**The runtime error** is caused by a nil pointer dereference, to fix this go to the generated file
`javascript_lexer.go` and replace use of `*JavaScriptLexerBase` with `JavaScriptLexerBase`
(i.e., remove `*`).

The cause for this is that ANTLR4 automatically adds a pointer to the embedded `superClass` inside
`[GrammarName]Lexer` (`JavaScriptLexer` in this case) for the Go language. This behaviour is expected 
for single-file grammars (i.e. Both lexer and parser are inside the same file).
But it does not work for multi-file settings where the `superClass` option is set on the lexer grammar.

For more information how to use Go target, see
[documentation](https://github.com/antlr/antlr4/blob/master/doc/go-target.md).
