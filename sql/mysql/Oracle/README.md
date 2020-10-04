# Oracle's MySQL grammar

## General

Since more than a decade the MySQL GUI dev tools team at Oracle offers the open source
[MySQL Workbench](https://github.com/mysql/mysql-workbench) product, which uses ANTLR4 for all MySQL code parsing tasks.
This requires to translate all changes from the
[MySQL server grammar](https://github.com/mysql/mysql-server/blob/8.0/sql/sql_yacc.yy) to ANTLR4, which is an ongoing
effort to always stay up-to-date with the lastest and greatest server features.
The current grammar supports the server versions 5.6, 5.7 and 8.0.

## Correct and Flexible Parsing

To enable applications like MySQL Workbench to correctly parse MySQL code, some conditions must be respected, namely
the currently used MySQL **server version** and the active **SQL modes** (for example, to distinguish between
identifiers and double quoted strings, depending on the ANSI mode setting). Furthermore some specialities must be considered:
[**string literal concatenation** and **character set introducer**s (aka. underscore charsets or string repertoires)](https://dev.mysql.com/doc/refman/8.0/en/string-literals.html).

The server version and the SQL mode can be switched at runtime, enabling the use of a single parser with different
version/mode settings and to provide better error messages (like for [a feature that is only valid for a specific version](https://github.com/mysql/mysql-workbench/blob/8.0/modules/db.mysql.parser/src/mysql_parser_module.cpp#L391)).

## Using the Grammar

The [MySQL ANTLR4 grammar](https://github.com/mysql/mysql-workbench/tree/8.0/library/parsers/grammars) in
MySQL Workbench considers these conditions to deliver exact parsing results. Unfortunately, this is not possible
without using action code (and predicates) in the grammar, which are written in the target language of the host
application (C++). Additionally, the generated parser + lexer classes use a common base class to provide support
code and which contain fields for the current server version and active SQL modes (which are then used in the
predicates to guide the parsing process). You can find the few extra files in [another subfolder of the parser library](https://github.com/mysql/mysql-workbench/tree/8.0/library/parsers/mysql).

This code is well commented and easily translatable to other languages. Additionally, there are a number of pretty
useful helper functions that might be interesting for you when working with a MySQL parser, for example:

- Getting the original source text for a given rule context or between two tokens.
- Getting the text of a token with automatic string concatenation, as used for single and double quoted strings in MySQL.
- Dumping a (sub) parse tree from a rule context.
- Navigating back and forward, from a given parse tree reference.
- Finding a parse tree (rule context or terminal node) for a specific column/row position.
- Quickly finding the type of a query without doing a full parse run.
- Type checkers for a given token (identifier, relation, number, operator).

## Contributing

If you want to contribute bug fixes or enhancements open a pull request in the
[MySQL Workbench Github repository](https://github.com/mysql/mysql-workbench/pulls) or provide a patch
in the MySQL bug system. For questions join us in the [#workbench Slack channel](https://mysqlcommunity.slack.com/messages/C8THWN6PL).
