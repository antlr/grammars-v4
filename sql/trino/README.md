# Trino grammar for ANTLR4

An ANTLR4 grammar for Trino, formerly known as PrestoSQL.
This grammar is based on the actively maintained [Trino repository](https://github.com/trinodb/trino).

The lexer and parser are referenced from the [official base antlr file](https://github.com/trinodb/trino/blob/master/core/trino-grammar/src/main/antlr4/io/trino/grammar/sql/SqlBase.g4).
Changes are made to accommodate various language targets as the official one is being developed for Java only.

The examples are extracted from the [official test file](https://github.com/trinodb/trino/blob/master/core/trino-parser/src/test/java/io/trino/sql/parser/TestSqlParser.java).

## Reference

