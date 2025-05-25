-- Create Table With Index Option
CREATE TABLE dbo.TestTable (
  TableID uniqueidentifier NOT NULL,
  Value nvarchar(64) NOT NULL
  CONSTRAINT PK_TestTable_ID PRIMARY KEY (TableID) WITH (DATA_COMPRESSION = PAGE))
GO

-- Create Table With Index Option and Table Option
CREATE TABLE dbo.TestTable (
  TableID uniqueidentifier NOT NULL,
  Value nvarchar(64) NOT NULL,
  Name nvarchar(64) NOT NULL,
  ModifiedDateUTC SMALLDATETIME,
  CONSTRAINT UQ_TestTable_ID  UNIQUE (Value) WITH (DATA_COMPRESSION = PAGE),
  CONSTRAINT PK_TestTable_ID PRIMARY KEY (TableID, Name))
  WITH (DATA_COMPRESSION = PAGE)
GO

-- Alter table drop constraint in transaction
IF NOT EXISTS (SELECT * FROM sys.columns cols
  JOIN sys.types AS types ON cols.user_type_id = types.user_type_id
WHERE object_id = OBJECT_ID('dbo.TestTable')
  AND cols.name = 'ModifiedDateUTC'
  AND types.name = 'datetime')
BEGIN
  BEGIN TRAN
    ALTER TABLE dbo.TestTable DROP CONSTRAINT DF_ModifiedDate;
  COMMIT TRAN
END
GO

-- Alter table drop multiple constraints in transaction
IF NOT EXISTS (SELECT * FROM sys.columns cols
  JOIN sys.types AS types ON cols.user_type_id = types.user_type_id
WHERE object_id = OBJECT_ID('dbo.TestTable')
  AND cols.name = 'ModifiedDateUTC'
  AND types.name = 'datetime')
BEGIN
  BEGIN TRAN
    ALTER TABLE dbo.TestTable DROP CONSTRAINT DF_ModifiedDate;
    ALTER TABLE dbo.TestTable DROP CONSTRAINT UQ_TestTable_ID;
  COMMIT TRAN
END
GO

-- Alter table Add Constraint with Default
ALTER TABLE dbo.TestTable ADD CONSTRAINT DF_ModifiedDateUTC DEFAULT(GETUTCDATE()) FOR ModifiedDateUTC;
GO

-- Alter table Alter Column
ALTER TABLE dbo.TestTable ALTER COLUMN ModifiedDateUTC DATETIME
GO

-- Alter table Rebuild with Table Options
ALTER TABLE TestTable REBUILD WITH (DATA_COMPRESSION = PAGE, ONLINE=ON);
GO

-- Make column row GUID column
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC ADD ROWGUIDCOL;

-- Make column not row GUID column
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC DROP ROWGUIDCOL;

-- Mark column persisted
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC ADD PERSISTED;

-- Mark column not persisted
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC DROP PERSISTED;

-- Mark column as not for replication
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC ADD NOT FOR REPLICATION;

-- Mark column as for replication
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC DROP NOT FOR REPLICATION;

-- Mark column sparse
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC ADD SPARSE;

-- Mark column non-sparse
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC DROP SPARSE;

-- Mark column hidden
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC ADD HIDDEN;

-- Mark column not hidden
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC DROP HIDDEN;

-- Mark column masked
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC ADD MASKED;

-- Mark column not masked
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC DROP MASKED;

-- Mark column masked with function
ALTER TABLE TestTable ALTER COLUMN ModifiedDateUTC ADD MASKED WITH (FUNCTION = 'default()');

-- Create Table with Specified Order in Constraint
CREATE TABLE [dbo].[TestTable] (
  TableID UNIQUEIDENTIFIER NOT NULL,
  Value NVARCHAR(64) NOT NULL,
  Name NVARCHAR(64) NOT NULL,
  CONSTRAINT [PK_TestTable_Value] PRIMARY KEY CLUSTERED (
    [TableID] ASC,
    [Value] ASC))
GO

-- Create Table with NOT NULL and DEFAULT Constraint
CREATE TABLE [dbo].[TestTable] (
  TableID UNIQUEIDENTIFIER NOT NULL,
  Name NVARCHAR(64) NOT NULL,
  Value BIT CONSTRAINT DF_TestTable_Value NOT NULL DEFAULT (0))
  WITH (DATA_COMPRESSION = PAGE)
GO

-- Create Table with indices
CREATE TABLE [dbo].[TestTable] (
  Name NVARCHAR(64) NOT NULL,
  K NVARCHAR(64) NOT NULL,
  Value NVARCHAR(64) NOT NULL,
  Guid NVARCHAR(64) NOT NULL,
  index ix_name UNIQUE CLUSTERED (Name),
  INDEX ix_k UNIQUE NONCLUSTERED (K),
  INDEX ix_value NONCLUSTERED (Value),
  INDEX ix_guid UNIQUE (Guid))
GO

-- Create Table with column store index
CREATE TABLE [dbo].[TestTable] (
  Name NVARCHAR(64) NOT NULL,
  Value NVARCHAR(64) NOT NULL,
  Guid NVARCHAR(64) NOT NULL,
  index ix_store CLUSTERED COLUMNSTORE,
  index ix_value_guid COLUMNSTORE (Value, Guid),
  index ix_value_name NONCLUSTERED (Value, Name)
)

-- Drop Column
IF EXISTS(SELECT * FROM sys.columns WHERE NAME = N'Name' AND Object_ID = Object_ID(N'dbo.TestTable'))
BEGIN
  ALTER TABLE dbo.TestTable
  DROP COLUMN Name
END
GO

-- Drop Columns
ALTER TABLE dbo.TestTable
  DROP COLUMN Name, Value
GO

-- Drop Index Using Fully Qualified Name
DROP INDEX dbo.TestTable.UIX_TestTable_Name_Value
GO

-- Alter Table Add Column With Default Constraint First
ALTER TABLE TestTable
  ADD Value BIT
  CONSTRAINT DF_TestTable_Value DEFAULT(0) NOT NULL
GO

-- Alter Table Add Column With Null Constraint First
ALTER TABLE TestTable
  ADD Value BIT
  CONSTRAINT DF_TestTable_Value NOT NULL DEFAULT(0)
GO

-- Alter Table Add Constraint To Column
ALTER TABLE dbo.TestTable 
  ADD CONSTRAINT DF_TestTable_Value DEFAULT(0) 
  FOR Value
GO

-- Alter Table Add Constraint To Column
ALTER TABLE dbo.TestTable
  ADD CONSTRAINT DF_TestTable_Value DEFAULT(0)
  FOR Value
  WITH VALUES
GO

-- Alter Table Add Constraint To Column with extra parentheses
ALTER TABLE dbo.TestTable
  ADD CONSTRAINT DF_TestTable_Value DEFAULT((0))
  FOR Value
GO
ALTER TABLE dbo.TestTable
  ADD CONSTRAINT DF_TestTable_Value DEFAULT(((((0)))))
  FOR Value
GO

-- Alter Table Add Constraint With String Concatenation
ALTER TABLE dbo.TestTable
  ADD CONSTRAINT DF_Name
  DEFAULT('NONE_' + CONVERT(NVARCHAR(40),NEWID())) 
  FOR Name
GO

ALTER TABLE dbo.TestTable  WITH NOCHECK ADD  CONSTRAINT [FK_NAME] FOREIGN KEY([StateId])
REFERENCES dbo.TableState (ID)
ON UPDATE CASCADE
ON DELETE CASCADE
GO

ALTER TABLE [dbo].[TestTable] WITH CHECK ADD FOREIGN KEY([StateId])
REFERENCES [dbo].[TableState] ([Id])
GO

ALTER TABLE [dbo].[TestTable] ADD  CONSTRAINT [constraintName]  DEFAULT (NEXT VALUE FOR [dbo].[sequence]) FOR [ID]
GO

ALTER TABLE [dbo].[TestTable]  WITH CHECK ADD  CONSTRAINT [constraintName] CHECK  ([StartDate] < [EndDate])
GO

ALTER TABLE [dbo].[TestTable] NOCHECK CONSTRAINT [constraintName]
GO

-- Alter Table Switch Partition
ALTER TABLE Source SWITCH PARTITION 1 TO Target PARTITION 1
GO
ALTER TABLE Source SWITCH TO Target PARTITION 1
GO
ALTER TABLE Source SWITCH PARTITION 1 TO Target WITH WAIT_AT_LOW_PRIORITY ( MAX_DURATION = 0 minutes, ABORT_AFTER_WAIT = NONE)
GO
ALTER TABLE Source SWITCH TO Target PARTITION $PARTITION.PF_TEST_DT( '20121201' )
GO
-- Constraints
ALTER TABLE #t ADD CONSTRAINT PL_t PRIMARY KEY CLUSTERED (IDCol)
  WITH (
    ALLOW_PAGE_LOCKS = ON,
    ALLOW_ROW_LOCKS = ON)
  ON "default"
GO
ALTER TABLE #t ADD CONSTRAINT [PK_#t] PRIMARY KEY CLUSTERED
(
  Col1 asc,
  Col2 asc
) WITH FILLFACTOR = 90 ON [PRIMARY]
GO
