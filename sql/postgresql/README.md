# PostgreSQL grammar

PostgreSQL sql/pgsql grammar based on sources from https://github.com/postgres/postgres/tree/master/src/
Sample scripts taken from https://github.com/postgres/postgres/tree/master/src/test/regress/sql
Lexer taken from https://github.com/tunnelvisionlabs/antlr4-grammar-postgresql/blob/master/src/com/tunnelvisionlabs/postgresql/PostgreSqlLexer.g4

Utility classes/methods writen in c#.

Not all rules yet balanced so produced tree can be incorrect, however, the script still be parsed with no errors.

examples: sql scripts which passed test without errors
examples.errors: sql scripts which still have parsing errors
