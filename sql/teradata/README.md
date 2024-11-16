# Teradata SQL Grammar  

An [ANTLR4](https://www.antlr.org/) grammar for Teradata SQL. Based on a grammar of Teradata Database version 17.10.  

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

## Code style guidelines  

Please, follow simple rules in this section when contributing to the grammar.  

### Naming  
- Rules for SQL statements should have suffix `_stat`.  
- Please, do not use obscure abbreviations, ambiguous names and names similar to existing ones.

