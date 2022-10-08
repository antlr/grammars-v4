create procedure a as begin
	select 1
end
go
create procedure b as begin
	select 2
end
go

-- call stored procedure, name in local var
IF EXISTS (SELECT * FROM sys.objects WHERE type = 'P' AND name = 'TestSproc')
  DROP PROCEDURE dbo.TestSproc
GO

CREATE PROCEDURE dbo.TestSproc @Name nvarchar(30) AS SELECT @Name RETURN
GO

DECLARE @SprocName nvarchar(64) = 'TestSproc';
DECLARE @TestName nvarchar(64) = 'Foo'
DECLARE @Result nvarchar(64)
EXEC @Result = @SprocName @Name = @TestName;
SELECT @Result
GO


CREATE OR ALTER PROC What_DB_is_this
AS
SELECT DB_NAME() AS ThisDB;
GO

CREATE procedure [schema].[stp] 
	@id int = 0
AS
BEGIN

SELECT * FROM (
	SELECT id
	FROM table1

	UNION ALL

	SELECT  id
	FROM table2
	
	UNION ALL

	SELECT  id
	FROM table3
) AS RESULT
END
GO

CREATE PROCEDURE [DBO].[EXECUTEOLAP] @MY_PARAM NVARCHAR(MAX)
WITH EXECUTE AS OWNER
AS EXTERNAL NAME [MY_ASSEMBLY_NAME].[MY_NAMESPACE.MY_CLASS].[MY_METHOD]
go

create or replace procedure dbo.a_procedure
as
select * from a_table t
go

