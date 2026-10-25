-- Create a nonclustered index on a table or view  
CREATE INDEX i1 ON t1 (col1);  

--Create a clustered index on a table and use a 3-part name for the table  
CREATE CLUSTERED INDEX i1 ON d1.s1.t1 (col1);  

-- Create a nonclustered index with a unique constraint on 3 columns and specify the sort order for each column  
CREATE UNIQUE INDEX i1 ON t1 (col1 DESC, col2 ASC, col3 DESC);  

-- Create a nonclustered index with a unique constraint on 3 columns and specify the sort order for each column  
CREATE UNIQUE INDEX i1 ON t1 (col1 DESC, col2 ASC, col3 DESC);  

CREATE NONCLUSTERED INDEX IX_Address_PostalCode  
    ON Person.Address (PostalCode)  
    INCLUDE (AddressLine1, AddressLine2, City, StateProvinceID);  

-- filtered index
CREATE NONCLUSTERED INDEX IX_BillOfMaterials_ComponentID
    ON Production.BillOfMaterials (ComponentID, StartDate)
    WHERE EndDate IS NOT NULL ;

-- this feature will be deprecated in future version of tsql 
-- but need to support in case someone want to capture this and throw a warning
DROP INDEX tbl.idx_tbl_col;

DROP INDEX IX_ProductVendor_BusinessEntityID
    ON Purchasing.ProductVendor;
GO
CREATE CLUSTERED COLUMNSTORE INDEX [indexName] ON [dbo].[table] WITH (DROP_EXISTING = OFF) ON [filegroup_name]
GO
CREATE NONCLUSTERED INDEX [indexName] ON [dbo].[table]
(
	[Column1] ASC
)
INCLUDE ([ColumnName2],[ColumnName3]) WITH (PAD_INDEX = OFF, STATISTICS_NORECOMPUTE = OFF, SORT_IN_TEMPDB = OFF, DROP_EXISTING = OFF, ONLINE = OFF, ALLOW_ROW_LOCKS = ON, FILLFACTOR = 95) ON [filegroup_name]
GO

ALTER INDEX [indexname] ON [schema].[tableName] DISABLE
GO

ALTER INDEX [indexName] ON [dbo].[tbl] REBUILD PARTITION = ALL
GO

ALTER INDEX index1 ON table1 REBUILD;

ALTER INDEX ALL ON table1 REBUILD;

ALTER INDEX ALL ON dbo.table1 REBUILD;

CREATE CLUSTERED COLUMNSTORE INDEX idxcci_cci_target ON cci_target;

ALTER INDEX idxcci_cci_target ON cci_target REORGANIZE WITH (COMPRESS_ALL_ROW_GROUPS = ON);

ALTER INDEX idxcci_cci_target ON cci_target REORGANIZE WITH (COMPRESS_ALL_ROW_GROUPS = ON);

ALTER INDEX cci_FactInternetSales2 ON FactInternetSales2 REORGANIZE;

-- REORGANIZE a specific partition
ALTER INDEX cci_FactInternetSales2 ON FactInternetSales2 REORGANIZE PARTITION = 0;

ALTER INDEX cci_FactInternetSales2 ON FactInternetSales2 REORGANIZE WITH (COMPRESS_ALL_ROW_GROUPS = ON);

ALTER INDEX cci_FactInternetSales2 ON FactInternetSales2 REORGANIZE PARTITION = 0 WITH (COMPRESS_ALL_ROW_GROUPS = ON);

CREATE CLUSTERED COLUMNSTORE INDEX cci_FactInternetSales2
ON dbo.FactInternetSales2;

ALTER INDEX cci_FactInternetSales2 ON FactInternetSales2 REBUILD;

ALTER INDEX cci_fact3
ON fact3
REBUILD PARTITION = 12;

CREATE CLUSTERED INDEX cci_SimpleTable ON SimpleTable (ProductKey);

CREATE CLUSTERED COLUMNSTORE INDEX cci_SimpleTable
ON SimpleTable
WITH (DROP_EXISTING = ON);

--Compress the table further by using archival compression.
ALTER INDEX cci_SimpleTable ON SimpleTable
REBUILD
WITH (DATA_COMPRESSION = COLUMNSTORE_ARCHIVE);
GO

ALTER INDEX cci_SimpleTable ON SimpleTable
REBUILD
WITH (DATA_COMPRESSION = COLUMNSTORE);
GO

ALTER INDEX PK_Employee_EmployeeID ON HumanResources.Employee REBUILD;

ALTER INDEX ALL ON Production.Product
REBUILD WITH (FILLFACTOR = 80, SORT_IN_TEMPDB = ON, STATISTICS_NORECOMPUTE = ON);

ALTER INDEX ALL ON Production.Product
REBUILD WITH
(
    FILLFACTOR = 80,
    SORT_IN_TEMPDB = ON,
    STATISTICS_NORECOMPUTE = ON,
    ONLINE = ON ( WAIT_AT_LOW_PRIORITY ( MAX_DURATION = 4 MINUTES, ABORT_AFTER_WAIT = BLOCKERS ) ),
    DATA_COMPRESSION = ROW
);

ALTER INDEX PK_ProductPhoto_ProductPhotoID ON Production.ProductPhoto REORGANIZE WITH (LOB_COMPACTION = ON);

ALTER INDEX AK_SalesOrderHeader_SalesOrderNumber ON
    Sales.SalesOrderHeader
SET (
    STATISTICS_NORECOMPUTE = ON,
    IGNORE_DUP_KEY = ON,
    ALLOW_PAGE_LOCKS = ON
    ) ;
GO

ALTER INDEX IX_Employee_ManagerID ON HumanResources.Employee DISABLE;

ALTER INDEX PK_Department_DepartmentID ON HumanResources.Department DISABLE;

ALTER INDEX PK_Department_DepartmentID ON HumanResources.Department REBUILD;

--Rebuild only partition 5.
ALTER INDEX IX_TransactionHistory_TransactionDate
ON Production.TransactionHistory
REBUILD Partition = 5
   WITH (ONLINE = ON (WAIT_AT_LOW_PRIORITY (MAX_DURATION = 10 minutes, ABORT_AFTER_WAIT = SELF)));
GO

ALTER INDEX IX_INDEX1
ON T1
REBUILD
WITH (DATA_COMPRESSION = PAGE);
GO

ALTER INDEX IX_INDEX1
ON T1
REBUILD
WITH (XML_COMPRESSION = ON);
GO

ALTER INDEX test_idx on test_table REBUILD WITH (ONLINE = ON, MAXDOP = 1, RESUMABLE = ON) ;

ALTER INDEX test_idx on test_table REBUILD WITH (ONLINE = ON, RESUMABLE = ON, MAX_DURATION = 240) ;

ALTER INDEX test_idx on test_table PAUSE ;

ALTER INDEX test_idx on test_table RESUME WITH (MAXDOP = 4) ;

ALTER INDEX test_idx on test_table
      RESUME WITH (MAXDOP = 2, MAX_DURATION = 240 MINUTES,
      WAIT_AT_LOW_PRIORITY (MAX_DURATION = 10, ABORT_AFTER_WAIT = BLOCKERS)) ;

ALTER INDEX test_idx on test_table ABORT ;
