# Description

This is a community supported grammar file for t-sql, it is cool, yet not very complete so far because a grammar reference of T-SQL is hard to find. MS website has grammar reference for different statements, but it's not a complete file, so we try hard to stick to the references we could find, and bit by bit make it a more complete grammar.



> Please remove the todo items as your Pull Request solves the problem, thanks!

# TODO



## Empty statement

Test SQL:

```sql
;;; -- MS-SQL allows empty sql-clauses like this, we should support it.
```



### Create/alter/drop funtction

Grammar should support function creation/alternation/deletion

Test SQL:

```sql
Create Function dbo.FooBar(
	@p1 nVarchar(4000)
)
Returns int
As
Begin
  return 123;
end
Go

Alter Function dbo.FooBar(
	@p2 int
)
Returns nVarchar(4000)
As
Begin
  return N'1';
end
GO

Drop function dbo.FooBar
GO

```





## Update statistics  

Ref: https://msdn.microsoft.com/en-us/library/ms187348.aspx

We already have create/drop statistic statements, we are missing the update.

Test SQL:

```sql
UPDATE STATISTICS #TableName;
```





## DBCC statements

Ref: https://msdn.microsoft.com/en-us/library/ms176057.aspx

Test SQL:

```sql
DBCC DROPCLEANBUFFERS
DBCC FREEPROCCACHE
DBCC CHECKIDENT (TableA, reseed, 1)  
```





## Alter table (add column, NOCHECK)

Ref: https://msdn.microsoft.com/en-us/library/ms190273.aspx

Missing add column sub-rule

Missing NOCHECK in with sub-rule

Test SQL:

```sql
alter table [dbo].[TableA] ALTER COLUMN ColB nvarchar(50)   --test alter subrule

--Test a more complete alter table statement
ALTER TABLE dbo.TableA
    WITH NOCHECK
    ADD CONSTRAINT FK_ContrainName
	FOREIGN KEY (Col1,Col2)
	REFERENCES TableB (Col1,Col2)

```

