# An ANTLR4 grammar for T-SQL

This is a community supported grammar file for t-sql, it is cool, yet not very complete so far because a grammar reference of T-SQL is hard to find. MS website has grammar reference for different statements, but it's not a complete file, so we try hard to stick to the references we could find, and bit by bit make it a more complete grammar.

The reference for the Adaptive Server Enterprise variant of TSql is [online](http://infocenter.sybase.com/help/topic/com.sybase.dc36272_1251/pdf/commands.pdf).

## Usage, important note

As SQL grammar are normally not case sensitive but this grammar implementation is, you must use a custom [character stream](https://github.com/antlr/antlr4/blob/master/runtime/Java/src/org/antlr/v4/runtime/CharStream.java) that converts all characters to uppercase before sending them to the lexer.

You could find more information [here](https://github.com/antlr/antlr4/blob/master/doc/case-insensitive-lexing.md#custom-character-streams-approach) with implementations for various target languages.

