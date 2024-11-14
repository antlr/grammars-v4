# PostgreSQL grammar

PostgreSQL sql/pgsql grammar based on the
Bison grammar at the Github mirror: [src/backend/parser/gram.y](https://github.com/postgres/postgres/blob/9be4e5d293b554d8a0800790c57fc707a3b5cf0f/src/backend/parser/gram.y).

Lexer was derived from https://github.com/tunnelvisionlabs/antlr4-grammar-postgresql/blob/master/src/com/tunnelvisionlabs/postgresql/PostgreSqlLexer.g4.

Sample SQL tests are derived from the sources
at https://github.com/postgres/postgres/tree/master/src/test/regress/sql.

# Target Agnostic

This grammar is target agnostic. Therefore the grammar contains certain patterns
that may not work for a give target. If the file transformGrammar.py exists
for the target, run `python transformGrammar.py` from the command line to alter the
.g4 files for the specific target.

# Issues

* There are two warnings from the Antlr Tool in the lexer grammar
(`non-fragment lexer rule AfterEscapeStringConstantMode_NotContinued can match the empty string` and `non-fragment lexer rule AfterEscapeStringConstantWithNewlineMode_NotContinued can match the empty string`).
* The grammar is ambiguous.

