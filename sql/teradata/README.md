# Teradata SQL Grammar  

An [ANTLR4](https://www.antlr.org/) grammar for Teradata SQL. Based on a grammar of Teradata Database version 17.10.  
This grammar was written using vendor's official documentation:  
- [SQL Fundamentals](https://docs.teradata.com/r/SQL-Fundamentals/July-2021)  
- [SQL Data Types and Literals](https://docs.teradata.com/r/SQL-Data-Types-and-Literals/July-2021)
- [SQL Functions, Expressions, and Predicates](https://docs.teradata.com/r/SQL-Functions-Expressions-and-Predicates/July-2021)
- [SQL Operators and User-Defined Functions](https://docs.teradata.com/r/SQL-Operators-and-User-Defined-Functions/July-2021)
- [SQL Date and Time Functions and Expressions](https://docs.teradata.com/r/SQL-Date-and-Time-Functions-and-Expressions/July-2021)
- [SQL Data Manipulation Language](https://docs.teradata.com/r/SQL-Data-Manipulation-Language/July-2021)
- [SQL Data Definition Language Syntax and Examples](https://docs.teradata.com/r/SQL-Data-Definition-Language-Syntax-and-Examples/July-2021)
- [SQL Data Definition Language Detailed Topics](https://docs.teradata.com/r/SQL-Data-Definition-Language-Detailed-Topics/July-2021)
- [SQL Stored Procedures and Embedded SQL](https://docs.teradata.com/r/SQL-Stored-Procedures-and-Embedded-SQL/July-2021)
- [SQL Data Control Language](https://docs.teradata.com/r/SQL-Data-Control-Language/July-2021)  

Work in progress!

A few things to consider if you are going to use this parser grammar to check semantics of SQL statements:  
- In `CREATE/REPLACE MACRO`, according to documentation, 
> SQL DCL and DDL statements for administration of row level security are not allowed.  
- There are no distinct parser rules for `SELECT/SELECT AND CONSUME` and `SELECT...INTO/SELECT AND CONSUME...INTO`,
so you should check for mandatory `into_clause` when standalone `select_stat` is used inside procedure definition.
- You should check value format in parsed `interval_literal`.  
- And many more.

## Possible performance issues  
- `group_by_clause` rule could cause performance penalty due to inherent ambiguity 
in `group_by_spec` and `ordinary_grouping_set` rules.

## Roadmap
- 2.0.0 - rules for all SQL statements should be implemented.  
- 3.0.0 - all sub-rules should have labels.  
- 4.0.0 - all rules should consist of smaller rules appropriate for easy analysis and interpretation 
of interesting parts of the parsed SQL.  

## Naming  
- Rules for SQL statements should have suffix `_stat`.  
- Please, do not use obscure abbreviations, ambiguous names and names similar to existing ones.

## Examples
Most of the examples were taken from the official documentation.
Each script could contain multiple statements, so you may wish to use the top-level `sql_script` rule to parse them.

## Reference
* [pldb](http://pldb.info/concepts/teradata)

