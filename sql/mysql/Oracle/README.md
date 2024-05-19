# Oracle's MySQL Grammar

## General

For more than a decade, the MySQL GUI development tools team at Oracle has provided the open source
[MySQL Workbench](https://github.com/mysql/mysql-workbench), which uses ANTLR4 for all MySQL code parsing tasks. This requires to translate all changes from the
[MySQL server grammar](https://github.com/mysql/mysql-server/blob/8.0/sql/sql_yacc.yy) to ANTLR4, which is an ongoing effort to always stay up-to-date with the lastest and greatest server features (like the Multi Language Extension (MLE) for stored routines). The current grammar supports all MySQL versions starting with 8.0.

Meanwhile, development focus has been shifted to the [MySQL Shell for VS Code extension](https://marketplace.visualstudio.com/items?itemName=Oracle.mysql-shell-for-vs-code), which is the original source of the grammar you can find here.

Parser generated from this grammar are very fast (given the high ambiquity of the MySQL language). For details see the [ANTLR4 runtime benchmarks](https://github.com/mike-lischke/antlr4-runtime-benchmarks/tree/main/src/mysql) repository.

Like all of Oracle's open source, this grammar is released under the GPLv2.

## Correct and Flexible Parsing

In order for applications like MySQL Shell for VS Code to parse MySQL code correctly, some conditions must be considered, namely the currently used MySQL **server version** and the active **SQL modes** (e.g. to distinguish between identifiers and double-quoted strings, depending on the ANSI mode setting). There are also some peculiarities to consider: 

- [String literal concatenation and Character set introducers (aka underscore charsets or string repertoires)](https://dev.mysql.com/doc/refman/8.0/en/string-literals.html).
- [Keyword after dot](https://dev.mysql.com/doc/refman/8.0/en/keywords.html)
- [Built-in function name parsing](https://dev.mysql.com/doc/refman/8.0/en/function-resolution.html). The IGNORE_SPACE SQL mode is properly handled.
- [Version Comments](https://dev.mysql.com/doc/refman/8.0/en/comments.html), like `CREATE TABLE t1(a INT, KEY (a)) /*!50110 KEY_BLOCK_SIZE=1024 */;`

The server version and SQL mode can be toggled at runtime, allowing the use of a single parser with different version/mode settings and providing better error messages (like for [a feature that is only valid for a specific version](https://github.com/mysql/mysql-shell-plugins/blob/master/gui/frontend/src/parsing/mysql/MySQLErrorListener.ts#L109)).

String repertoires require a list of character set identifiers, which must be provided by your implementation. You can get a list of available character sets by running `show character set`.

## Using the Grammar

To provide the full feature set the MySQL grammar needs some support code, which is implemented in base classes for both, the MySQL Parser (named `MySQLBaseRecognizer`) and the MySQL Lexer (named `MySQLBaseLexer`). You can find a TypeScript implementation of both classes in the TypeScript/ folder, which should be easy to port over to different runtime languages.

This folder also contains a demo script that shows how to set up the MySQL lexer and parser and parse some input. It needs the TS runtime antlr4ng (and some additional modules to allow running the demo). For this run the node module installation in the TypeScript/ folder:

```bash
npm i
```

After that you can generate the (TypeScript) parser and lexer files by running:

```bash
npm run generate
```

A new folder is created name `generated`, which contains the new files. Now the demo is ready for execution:

```bash
npm run demo
```

It will run a simple MySQL query and prints its parse tree.
