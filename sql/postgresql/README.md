# postgresql grammar

postgresql sql/pgsql grammar based on sources from https://github.com/postgres/postgres/tree/master/src/
test scripts taken from https://github.com/postgres/postgres/tree/master/src/test/regress/sql
lexer taken from https://github.com/tunnelvisionlabs/antlr4-grammar-postgresql/blob/master/src/com/tunnelvisionlabs/postgresql/PostgreSqlLexer.g4

Utility classes/methods writen in c#.
Solution contains unit tests and sandbox.

Not all rules yet balanced so produced tree can be incorrect, however, the script still be parsed with no errors.