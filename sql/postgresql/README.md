# PostgreSQL grammar

PostgreSQL sql/pgsql grammar based on sources from https://github.com/postgres/postgres/tree/master/src/
Sample scripts taken from https://github.com/postgres/postgres/tree/master/src/test/regress/sql
Lexer taken from https://github.com/tunnelvisionlabs/antlr4-grammar-postgresql/blob/master/src/com/tunnelvisionlabs/postgresql/PostgreSqlLexer.g4

Not all rules are balanced, that's why the produced trees may be incorrect.
However, the script still be parsed with no errors.
