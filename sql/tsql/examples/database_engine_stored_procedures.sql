-- sp_executesql
DECLARE @IntVariable INT;
DECLARE @SQLString NVARCHAR(500);
DECLARE @ParmDefinition NVARCHAR(500);

-- Build the SQL string one time.
SET @SQLString =
     N'SELECT BusinessEntityID, NationalIDNumber, JobTitle, LoginID
       FROM AdventureWorks2012.HumanResources.Employee
       WHERE BusinessEntityID = @BusinessEntityID';
SET @ParmDefinition = N'@BusinessEntityID tinyint';
-- Execute the string with the first parameter value.
SET @IntVariable = 197;
EXECUTE sp_executesql @SQLString, @ParmDefinition,
                      @BusinessEntityID = @IntVariable;
 -- Execute the same string with the second parameter value.
SET @IntVariable = 109;
EXECUTE sp_executesql @SQLString, @ParmDefinition,
                      @BusinessEntityID = @IntVariable;

GO

DECLARE @IntVariable INT;
DECLARE @SQLString NVARCHAR(500);
DECLARE @ParmDefinition NVARCHAR(500);
DECLARE @max_title VARCHAR(30);

SET @IntVariable = 197;
SET @SQLString = N'SELECT @max_titleOUT = max(JobTitle)
   FROM AdventureWorks2012.HumanResources.Employee
   WHERE BusinessEntityID = @level';
SET @ParmDefinition = N'@level TINYINT, @max_titleOUT VARCHAR(30) OUTPUT';

EXECUTE sp_executesql @SQLString, @ParmDefinition, @level = @IntVariable, @max_titleOUT=@max_title OUTPUT;
SELECT @max_title;
GO
