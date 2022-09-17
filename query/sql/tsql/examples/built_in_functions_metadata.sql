USE AdventureWorks2012;
GO
IF APP_NAME() = 'Microsoft SQL Server Management Studio - Query'
PRINT 'This process was started by ' + APP_NAME() + '. The date is ' + CONVERT ( VARCHAR(100) , GETDATE(), 101) + '.';
ELSE
PRINT 'This process was started by ' + APP_NAME() + '. The date is ' + CONVERT ( VARCHAR(100) , GETDATE(), 102) + '.';
GO

USE AdventureWorks2012;
GO
BEGIN TRAN;
DECLARE @result INT;
EXEC @result=sp_getapplock
    @DbPrincipal='public',
    @Resource='Form1',
    @LockMode='Shared',
    @LockOwner='Transaction';
SELECT APPLOCK_MODE('public', 'Form1', 'Transaction');
GO

Use AdventureWorks2012;
GO
BEGIN TRAN;
SELECT APPLOCK_MODE('public', 'Form1', 'Transaction');
--Result set: NoLock

SELECT APPLOCK_TEST('public', 'Form1', 'Shared', 'Transaction');
--Result set: 1 (Lock is grantable.)

SELECT APPLOCK_TEST('public', 'Form1', 'Exclusive', 'Transaction');
--Result set: 0 (Lock is not grantable.)
GO

EXEC sp_releaseapplock @Resource='Form1', @DbPrincipal='public';
GO

SELECT APPLOCK_TEST('public', 'Form1', 'Exclusive', 'Transaction');
--Result set: '1' (The lock is grantable.)
GO

COMMIT TRAN;
GO

USE AdventureWorks2012;
GO
SELECT ASSEMBLYPROPERTY ('HelloWorld' , 'PublicKey');

USE AdventureWorks2012;
GO
CREATE TABLE t1(c1 VARCHAR(40), c2 NVARCHAR(40) );
GO
SELECT COL_LENGTH('t1','c1')AS 'VarChar',
      COL_LENGTH('t1','c2')AS 'NVarChar';
GO
DROP TABLE t1;

-- Uses AdventureWorks

SELECT COL_NAME(OBJECT_ID('dbo.FactResellerSales'), 1) AS FirstColumnName,
COL_NAME(OBJECT_ID('dbo.FactResellerSales'), 2) AS SecondColumnName;

USE AdventureWorks2012;
GO
SELECT COLUMNPROPERTY( OBJECT_ID('Person.Person'),'LastName','PRECISION')AS 'Column Length';
GO

SELECT DATABASEPROPERTYEX('AdventureWorks2014', 'IsAutoShrink');

SELECT
    DATABASEPROPERTYEX('AdventureWorks2014', 'Collation') AS Collation,
    DATABASEPROPERTYEX('AdventureWorks2014', 'Edition') AS Edition,
    DATABASEPROPERTYEX('AdventureWorks2014', 'ServiceObjective') AS ServiceObjective,
    DATABASEPROPERTYEX('AdventureWorks2014', 'MaxSizeInBytes') AS MaxSizeInBytes

SELECT DATABASEPROPERTYEX(DB_NAME(), 'Updateability');

SELECT DB_ID() AS [Database ID];
GO

SELECT DB_ID(N'AdventureWorks2008R2') AS [Database ID];
GO

DECLARE @db_id INT;
DECLARE @object_id INT;
SET @db_id = DB_ID(N'AdventureWorks2012');
SET @object_id = OBJECT_ID(N'AdventureWorks2012.Person.Address');
IF @db_id IS NULL
  BEGIN;
    PRINT N'Invalid database';
  END;
ELSE IF @object_id IS NULL
  BEGIN;
    PRINT N'Invalid object';
  END;
ELSE
  BEGIN;
    SELECT * FROM sys.dm_db_index_operational_stats(@db_id, @object_id, NULL, NULL);
  END;
GO

SELECT DB_ID();

SELECT DB_ID('AdventureWorksPDW2012');

SELECT DB_NAME() AS [Current Database];
GO

USE master;
GO
SELECT DB_NAME(3) AS [Database Name];
GO

SELECT DB_NAME(database_id) AS [Database], database_id
FROM sys.databases;

USE AdventureWorks2012;
GO
SELECT FILE_ID('AdventureWorks2012_Data')AS 'File ID';
GO

USE AdventureWorks2012;
GO
SELECT FILE_IDEX('AdventureWorks2012_Data') AS 'File ID';
GO

USE AdventureWorks2012;
GO
SELECT FILE_IDEX((SELECT TOP (1) name FROM sys.database_files WHERE type = 1)) AS 'File ID';
GO

SELECT FILE_IDEX((SELECT name FROM sys.master_files WHERE type = 4))
AS 'File_ID';

SELECT FILE_NAME(1) AS 'File Name 1', FILE_NAME(2) AS 'File Name 2';
GO

SELECT FILEGROUP_ID('PRIMARY') AS [Filegroup ID];
GO

SELECT FILEGROUP_NAME(1) AS [Filegroup Name];
GO

SELECT FILEGROUPPROPERTY('PRIMARY', 'IsDefault') AS 'Default Filegroup';
GO

SELECT FILEPROPERTY('AdventureWorks2012_Data', 'IsPrimaryFile')AS [Primary File];
GO

SELECT s.file_id,
       s.type_desc,
       s.name,
       FILEPROPERTYEX(s.name, 'BlobTier') AS BlobTier,
       FILEPROPERTYEX(s.name, 'AccountType') AS AccountType,
       FILEPROPERTYEX(s.name, 'IsInferredTier') AS IsInferredTier,
       FILEPROPERTYEX(s.name, 'IsPageBlob') AS IsPageBlob
FROM sys.database_files AS s
WHERE s.type_desc IN ('ROWS', 'LOG');

USE AdventureWorks2012;
GO
SELECT fulltextcatalogproperty('Cat_Desc', 'ItemCount');
GO

SELECT fulltextserviceproperty('VerifySignature');

USE AdventureWorks2012;
GO
SELECT
    INDEX_COL (N'AdventureWorks2012.Sales.SalesOrderDetail', 1,1) AS
        [Index Column 1],
    INDEX_COL (N'AdventureWorks2012.Sales.SalesOrderDetail', 1,2) AS
        [Index Column 2]
;
GO

USE AdventureWorks2012;
GO
SELECT
    INDEXKEY_PROPERTY(OBJECT_ID('Production.Location', 'U'),
        1,1,'ColumnId') AS [Column ID],
    INDEXKEY_PROPERTY(OBJECT_ID('Production.Location', 'U'),
        1,1,'IsDescending') AS [Asc or Desc order];

SELECT
    INDEXPROPERTY(OBJECT_ID('HumanResources.Employee'),
        'PK_Employee_BusinessEntityID','IsClustered')AS [Is Clustered],
    INDEXPROPERTY(OBJECT_ID('HumanResources.Employee'),
        'PK_Employee_BusinessEntityID','IndexDepth') AS [Index Depth],
    INDEXPROPERTY(OBJECT_ID('HumanResources.Employee'),
        'PK_Employee_BusinessEntityID','IndexFillFactor') AS [Fill Factor];

-- Uses AdventureWorks

SELECT
INDEXPROPERTY(OBJECT_ID('dbo.FactResellerSales'),
    'ClusteredIndex_6d10fa223e5e4c1fbba087e29e16a7a2','IsClustered') AS [Is Clustered],
INDEXPROPERTY(OBJECT_ID('dbo.FactResellerSales'),
    'ClusteredIndex_6d10fa223e5e4c1fbba087e29e16a7a2','IsColumnstore') AS [Is Columnstore Index],
INDEXPROPERTY(OBJECT_ID('dbo.FactResellerSales'),
    'ClusteredIndex_6d10fa223e5e4c1fbba087e29e16a7a2','IndexFillFactor') AS [Fill Factor];
GO

USE AdventureWorks2012;
GO
SELECT OBJECT_DEFINITION (OBJECT_ID(N'Person.uAddress')) AS [Trigger Definition];
GO

USE AdventureWorks2012;
GO
SELECT OBJECT_DEFINITION (OBJECT_ID(N'sys.sp_columns')) AS [Object Definition];
GO

USE master;
GO
SELECT OBJECT_ID(N'AdventureWorks2012.Production.WorkOrder') AS 'Object ID';
GO

USE AdventureWorks2012;
GO
IF OBJECT_ID (N'dbo.AWBuildVersion', N'U') IS NOT NULL
DROP TABLE dbo.AWBuildVersion;
GO

DECLARE @db_id INT;
DECLARE @object_id INT;
SET @db_id = DB_ID(N'AdventureWorks2012');
SET @object_id = OBJECT_ID(N'AdventureWorks2012.Person.Address');
IF @db_id IS NULL
  BEGIN;
    PRINT N'Invalid database';
  END;
ELSE IF @object_id IS NULL
  BEGIN;
    PRINT N'Invalid object';
  END;
ELSE
  BEGIN;
    SELECT * FROM sys.dm_db_index_operational_stats(@db_id, @object_id, NULL, NULL);
  END;
GO

SELECT OBJECT_ID('AdventureWorksPDW2012.dbo.FactFinance') AS 'Object ID';

USE AdventureWorks2012;
GO
SELECT DISTINCT OBJECT_NAME(object_id)
FROM master.sys.objects;
GO

USE AdventureWorks2012;
GO
SELECT DISTINCT OBJECT_SCHEMA_NAME(object_id, 1) AS schema_name
FROM master.sys.objects;
GO

USE AdventureWorks2012;
GO
DECLARE @MyID INT;
SET @MyID = (SELECT OBJECT_ID('AdventureWorks2012.Production.Product',
    'U'));
SELECT name, object_id, type_desc
FROM sys.objects
WHERE name = OBJECT_NAME(@MyID);
GO

SELECT DB_NAME(st.dbid) AS database_name,
    OBJECT_SCHEMA_NAME(st.objectid, st.dbid) AS schema_name,
    OBJECT_NAME(st.objectid, st.dbid) AS object_name,
    st.text AS query_text
FROM sys.dm_exec_query_stats AS qs
CROSS APPLY sys.dm_exec_sql_text(qs.sql_handle) AS st
WHERE st.objectid IS NOT NULL;
GO

SELECT QUOTENAME(DB_NAME(database_id))
    + N'.'
    + QUOTENAME(OBJECT_SCHEMA_NAME(object_id, database_id))
    + N'.'
    + QUOTENAME(OBJECT_NAME(object_id, database_id))
    , *
FROM sys.dm_db_index_operational_stats(NULL, NULL, NULL, NULL);
GO

SELECT name, object_id, type_desc
FROM sys.objects
WHERE name = OBJECT_NAME(274100017);

USE master;
GO
SELECT OBJECTPROPERTY(OBJECT_ID(N'AdventureWorks2012.HumanResources.vEmployee'), 'IsView');
GO

USE AdventureWorks2012;
GO
IF OBJECTPROPERTY (OBJECT_ID(N'Production.UnitMeasure'),'ISTABLE') = 1
   PRINT 'UnitMeasure is a table.'
ELSE IF OBJECTPROPERTY (OBJECT_ID(N'Production.UnitMeasure'),'ISTABLE') = 0
   PRINT 'UnitMeasure is not a table.'
ELSE IF OBJECTPROPERTY (OBJECT_ID(N'Production.UnitMeasure'),'ISTABLE') IS NULL
   PRINT 'ERROR: UnitMeasure is not a valid object.';
GO

USE AdventureWorks2012;
GO
SELECT OBJECTPROPERTY(OBJECT_ID('dbo.ufnGetProductDealerPrice'), 'IsDeterministic');
GO

-- Uses AdventureWorks

SELECT name, object_id, type_desc
FROM sys.objects
WHERE OBJECTPROPERTY(object_id, N'SchemaId') = SCHEMA_ID(N'dbo')
ORDER BY type_desc, name;
GO

-- Uses AdventureWorks

IF OBJECTPROPERTY (OBJECT_ID(N'dbo.DimReseller'),'ISTABLE') = 1
   SELECT 'DimReseller is a table.'
ELSE
   SELECT 'DimReseller is not a table.';
GO

USE master;
GO
SELECT OBJECTPROPERTYEX(OBJECT_ID(N'AdventureWorks2012.HumanResources.vEmployee'), 'IsView');
GO

USE AdventureWorks2012;
GO
CREATE SYNONYM MyEmployeeTable FOR HumanResources.Employee;
GO
SELECT OBJECTPROPERTYEX ( object_id(N'MyEmployeeTable'), N'BaseType')AS [Base Type];
GO

USE AdventureWorks2012;
GO
SELECT OBJECTPROPERTYEX(OBJECT_ID(N'HumanResources.Employee'), N'TABLEUPDATETRIGGERCOUNT');
GO

USE AdventureWorks2012;
GO
SELECT name, object_id, schema_id, type_desc
FROM sys.objects
WHERE OBJECTPROPERTYEX(object_id, N'TableHasForeignKey') = 1
ORDER BY name;
GO

-- Uses AdventureWorks

SELECT OBJECTPROPERTYEX ( object_id(N'dbo.DimReseller'), N'BaseType')AS BaseType;

-- Uses AdventureWorks

SELECT PARSENAME('AdventureWorksPDW2012.dbo.DimCustomer', 1) AS 'Object Name';
SELECT PARSENAME('AdventureWorksPDW2012.dbo.DimCustomer', 2) AS 'Schema Name';
SELECT PARSENAME('AdventureWorksPDW2012.dbo.DimCustomer', 3) AS 'Database Name';
SELECT PARSENAME('AdventureWorksPDW2012.dbo.DimCustomer', 4) AS 'Server Name';
GO

SELECT SCHEMA_ID();

SELECT SCHEMA_ID('dbo');

SELECT SCHEMA_NAME();

SELECT SCHEMA_NAME(1);

USE tempdb;
GO
CREATE TABLE TZ (
   Z_id  INT IDENTITY(1,1)PRIMARY KEY,
   Z_name VARCHAR(20) NOT NULL);

INSERT TZ
   VALUES ('Lisa'),('Mike'),('Carla');

SELECT * FROM TZ;

CREATE TABLE TY (
   Y_id  INT IDENTITY(100,5)PRIMARY KEY,
   Y_name VARCHAR(20) NULL);

INSERT TY (Y_name)
   VALUES ('boathouse'), ('rocks'), ('elevator');

SELECT * FROM TY;

CREATE TRIGGER Ztrig
ON TZ
FOR INSERT AS
   BEGIN
   INSERT TY VALUES ('')
   END;

INSERT TZ VALUES ('Rosalie');

SELECT SCOPE_IDENTITY() AS [SCOPE_IDENTITY];
GO
SELECT @@IDENTITY AS [@@IDENTITY];
GO

USE AdventureWorks2012;
GO
INSERT INTO Person.ContactType ([Name]) VALUES ('Assistant to the Manager');
GO
SELECT SCOPE_IDENTITY() AS [SCOPE_IDENTITY];
GO
SELECT @@IDENTITY AS [@@IDENTITY];
GO

INSERT INTO Sales.Customer ([TerritoryID],[PersonID]) VALUES (8,NULL);
GO
SELECT SCOPE_IDENTITY() AS [SCOPE_IDENTITY];
GO
SELECT @@IDENTITY AS [@@IDENTITY];
GO

SELECT
 SERVERPROPERTY('MachineName') AS ComputerName,
 SERVERPROPERTY('ServerName') AS InstanceName,
 SERVERPROPERTY('Edition') AS Edition,
 SERVERPROPERTY('ProductVersion') AS ProductVersion,
 SERVERPROPERTY('ProductLevel') AS ProductLevel;
GO

USE AdventureWorks2012;
GO
SELECT name AS stats_name,
    STATS_DATE(object_id, stats_id) AS statistics_update_date
FROM sys.stats
WHERE object_id = OBJECT_ID('Person.Address');
GO

USE AdventureWorks2012;
GO
SELECT name AS index_name,
    STATS_DATE(object_id, index_id) AS statistics_update_date
FROM sys.indexes
WHERE object_id = OBJECT_ID('Person.Address');
GO

--First, create a statistics object
USE AdventureWorksPDW2012;
GO
CREATE STATISTICS Customer_LastName_Stats
ON AdventureWorksPDW2012.dbo.DimCustomer (LastName)
WITH SAMPLE 50 PERCENT;
GO

--Return the date when Customer_LastName_Stats was last updated
USE AdventureWorksPDW2012;
GO
SELECT stats_id, name AS stats_name,
    STATS_DATE(object_id, stats_id) AS statistics_date
FROM sys.stats s
WHERE s.object_id = OBJECT_ID('dbo.DimCustomer')
    AND s.name = 'Customer_LastName_Stats';
GO

--Update Customer_LastName_Stats so it will have a different timestamp in the next query
GO
UPDATE STATISTICS dbo.dimCustomer (Customer_LastName_Stats);

--Return the date when Customer_LastName_Stats was last updated.
SELECT stats_id, name AS stats_name,
    STATS_DATE(object_id, stats_id) AS statistics_date
FROM sys.stats s
WHERE s.object_id = OBJECT_ID('dbo.DimCustomer')
    AND s.name = 'Customer_LastName_Stats';
GO

--Return the dates all statistics on the table were last updated.
SELECT stats_id, name AS stats_name,
    STATS_DATE(object_id, stats_id) AS statistics_date
FROM sys.stats s
WHERE s.object_id = OBJECT_ID('dbo.DimCustomer');
GO

USE AdventureWorksPDW2012;
GO
SELECT name AS index_name,
    STATS_DATE(object_id, index_id) AS statistics_update_date
FROM sys.indexes
WHERE object_id = OBJECT_ID('dbo.DimCustomer');
GO

/*
-- Full example can be uncommented when create type grammar is fixed.
USE tempdb;
GO
CREATE TYPE NewType FROM int;
GO
CREATE SCHEMA NewSchema;
GO
CREATE TYPE NewSchema.NewType FROM int;
GO
*/
SELECT TYPE_ID('NewType') AS [1 Part Data Type ID],
       TYPE_ID('NewSchema.NewType') AS [2 Part Data Type ID];
GO

SELECT TYPE_NAME(TYPE_ID('datetime')) AS [TYPE_NAME]
    ,TYPE_ID('datetime') AS [TYPE_ID];
GO

SELECT TYPE_NAME(TYPE_ID('datetime')) AS typeName,
    TYPE_ID('datetime') AS typeID FROM table1;

SELECT o.name AS obj_name, c.name AS col_name,
       TYPE_NAME(c.user_type_id) AS type_name
FROM sys.objects AS o
JOIN sys.columns AS c  ON o.object_id = c.object_id
WHERE o.name = 'Vendor'
ORDER BY col_name;
GO

SELECT TYPE_NAME(36) AS Type36, TYPE_NAME(239) AS Type239;
GO

SELECT * FROM sys.types;
GO

SELECT TYPEPROPERTY(SCHEMA_NAME(schema_id) + '.' + name, 'OwnerId') AS owner_id, name, system_type_id, user_type_id, schema_id
FROM sys.types;

SELECT TYPEPROPERTY( 'tinyint', 'PRECISION');
