# T-SQL (Transact-SQL, MSSQL) grammar

This is a community supported grammar file for t-sql, it is cool, yet not very complete 
so far because a grammar reference of T-SQL is hard to find.
MS website has grammar reference for different statements,
but it's not a complete file, so we try hard to stick to the references we could find,
and bit by bit make it a more complete grammar.

Use with Antlr 4.7
------------------
This grammar uses upper-case key-words only. That means, as sql is case-insensitive, that
it can’t be used with Antlr 4.7 directly. You need a CharStream that converts to 
upper-case for the lexer. An implementation for java can be found [on Github](https://github.com/antlr/antlr4/pull/2048/commits/8043e85d178a318172bc94639bd0c131d7e3c60c).

