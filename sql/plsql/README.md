An ANTLR4 grammar for PL/SQL

## Usage, important note

As SQL grammar are normally not case sensitive but this grammar implementation is, you must use a custom [character stream](https://github.com/antlr/antlr4/blob/master/runtime/Java/src/org/antlr/v4/runtime/CharStream.java) that converts all characters to uppercase before sending them to the lexer.

You could find more information [here](https://github.com/antlr/antlr4/blob/master/doc/case-insensitive-lexing.md#custom-character-streams-approach) with implementations for various target languages.

https://docs.oracle.com/en/database/oracle/oracle-database/21/sqlrf/Introduction-to-Oracle-SQL.html
