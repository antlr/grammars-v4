# PostgreSQL grammar

PostgreSQL sql/pgsql grammar based on the
Bison grammar at the Github mirror: [src/backend/parser/gram.y](https://github.com/postgres/postgres/blob/9be4e5d293b554d8a0800790c57fc707a3b5cf0f/src/backend/parser/gram.y).

Lexer was derived from https://github.com/tunnelvisionlabs/antlr4-grammar-postgresql/blob/master/src/com/tunnelvisionlabs/postgresql/PostgreSqlLexer.g4.

Sample SQL tests are derived from the sources
at https://github.com/postgres/postgres/tree/master/src/test/regress/sql.

# Issues

* There are two warnings from the Antlr Tool in the parser grammar
(`rule stmt_case contains an optional block with at least one alternative that can match an empty string`
and `rule stmt_return contains an optional block with at least one alternative that can match an empty string`).
* There are two warnings from the Antlr Tool in the lexer grammar
(`non-fragment lexer rule AfterEscapeStringConstantMode_NotContinued can match the empty string` and `non-fragment lexer rule AfterEscapeStringConstantWithNewlineMode_NotContinued can match the empty string`).
* The grammar is ambiguous.

