# An ANTLR4 grammar for Sybase (based upon T-SQL)

This is a community supported grammar file for Sybase t-sql. It is based upon the existing t-sql for MS SQL server and still contains structures not supported by Sybase. But it has been extended to support quoted strings variations supported by Sybase but not by MS.

The reference for the Adaptive Server Enterprise variant of TSql is [online](https://help.sap.com/docs/SAP_ASE/e0d4539d39c34f52ae9ef822c2060077/aafa4e4ebc2b101482b9d943f723d6e6.html).
Main difference(s):
- Support for double quoted strings in DDL and DML statements instead of single quoted strings. See example for execute in the [Sybase Reference Manual: Commands](https://help.sap.com/docs/SAP_ASE/e0d4539d39c34f52ae9ef822c2060077/ab2cd64abc2b101486cbba68f2c5a234.html): exec[ute] ("string" | char_variable [+ "string" | char_variable]...) and [Microsoft T-SQL doc](https://learn.microsoft.com/en-us/sql/t-sql/language-elements/execute-transact-sql?view=sql-server-ver16). The initial change was to support only the EXECUTE statement, but the difference seems to be everywhere. Microsoft documented to support only [single quoted strings](https://learn.microsoft.com/en-us/sql/t-sql/queries/select-transact-sql?view=sql-server-ver16). The Sybase SQL engine seem to allow usage of either single and double quoted strings in most DML and DDL statements.
- Microsoft only extensions and stored procedures have not been disabled as this would require to refactor the existing tsql structure into multiple files. (e.g. Azure only extensions)

The files:
- TSqlLexer.g4 and TSqlParser.g4 are copied from the [TSqlLexer.g4](../tsql/TSqlLexer.g4) and [TSqlParser.g4](../tsql/TSqlParser.g4) from the tsql grammar.
- SybaseLexer.g4 and SybaseParser.g4 are only overriding a few statements and rules from the [tsql grammar](../tsql).
