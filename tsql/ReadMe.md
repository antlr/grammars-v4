# Description
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
DBCC SHRINKLOG ( SIZE = DEFAULT );
DBCC SHRINKLOG;
DBCC PDW_SHOWSPACEUSED ( "AdventureWorksPDW2012..FactInternetSales" );
DBCC PROCCACHE  WITH NO_INFOMSGS
DBCC SHOWCONTIG (@id, @indid);
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



## PROC as alias of PROCEDURE

Ref: https://msdn.microsoft.com/en-us/library/ms174969.aspx



Our grammar now is aware of PROC-PROCEDURE interchangeability somewhere (create_procedure), while ignoring it somewhere else(drop_procedure). 

```sql
Drop PROC Pro1;
```





## Create Type (more complete)

We have create_type, but should be more complete as https://msdn.microsoft.com/en-us/library/ms175007.aspx

Now it does not support the **AS** keyword

Test SQL:

```sql
create type dbo.NumberList as table (number int)

```



## Grant_permission support more verbs

Ref: https://msdn.microsoft.com/en-us/library/mt204026.aspx

The current one does not support DELETE / UPDATE as in the ref.



```sql
GRANT UPDATE ON [dbo].[Table1] TO [ID2]
```



