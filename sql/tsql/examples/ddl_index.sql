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

