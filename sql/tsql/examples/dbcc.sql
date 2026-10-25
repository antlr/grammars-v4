-- Check the current database.
DBCC CHECKALLOC;
GO
-- Check the AdventureWorks2022 database.
DBCC CHECKALLOC (AdventureWorks2022);
GO
DBCC CHECKALLOC ([AdventureWorks2022]) WITH ALL_ERRORMSGS, ESTIMATEONLY;
GO
DBCC CHECKALLOC (0)
GO

-- Check the current database.
DBCC CHECKCATALOG;
GO
-- Check the AdventureWorks database.
DBCC CHECKCATALOG (AdventureWorks2022);
GO
DBCC CHECKCATALOG (AdventureWorks2022) WITH NO_INFOMSGS;
GO
DBCC CHECKCATALOG (0)
GO

DBCC CHECKCONSTRAINTS (Table1);
GO
DBCC CHECKCONSTRAINTS ('Production.CK_ProductCostHistory_EndDate');
GO
DBCC CHECKCONSTRAINTS WITH ALL_CONSTRAINTS;
GO
DBCC CHECKCONSTRAINTS (Table1) WITH NO_INFOMSGS, ALL_ERRORMSGS, ALL_CONSTRAINTS;
GO

-- Check the current database.
DBCC CHECKDB;
GO
-- Check the AdventureWorks2019 database without nonclustered indexes.
DBCC CHECKDB (AdventureWorks2019, NOINDEX);
GO
DBCC CHECKDB WITH NO_INFOMSGS;
GO
DBCC CHECKDB (0)
GO

DBCC CHECKFILEGROUP;
GO
DBCC CHECKFILEGROUP (1, NOINDEX);
GO
DBCC CHECKFILEGROUP (N'PRIMARY', REPAIR_ALLOW_DATA_LOSS) WITH NO_INFOMSGS, ALL_ERRORMSGS;
GO

DBCC CHECKTABLE ('HumanResources.Employee');
GO
DECLARE @indid int;
SET @indid = (SELECT index_id
              FROM sys.indexes
              WHERE object_id = OBJECT_ID('Production.Product')
                    AND name = 'AK_Product_Name');
DBCC CHECKTABLE ('Production.Product',@indid);
GO

DBCC CLEANTABLE (AdventureWorks2019, 'Production.Document', 0)
WITH NO_INFOMSGS;
GO
DBCC CLEANTABLE (AdventureWorks2022, 'dbo.CleanTableTest');
GO
DBCC CLEANTABLE (0, 'Production.Document')
GO

DBCC CLONEDATABASE (AdventureWorks2022, AdventureWorks_Clone);
GO
DBCC CLONEDATABASE (AdventureWorks2022, AdventureWorks_Clone) WITH NO_STATISTICS;
GO
DBCC CLONEDATABASE (AdventureWorks2022, AdventureWorks_Clone) WITH NO_STATISTICS, NO_QUERYSTORE;
GO
DBCC CLONEDATABASE (AdventureWorks2022, AdventureWorks_Clone) WITH VERIFY_CLONEDB;
GO
DBCC CLONEDATABASE (AdventureWorks2022, AdventureWorks_Clone) WITH VERIFY_CLONEDB, BACKUP_CLONEDB;
GO

DBCC DBREINDEX ('HumanResources.Employee', PK_Employee_BusinessEntityID, 80);
GO
DBCC DBREINDEX ('HumanResources.Employee', ' ', 70);
GO

DBCC xp_sample (FREE);
GO

DBCC DROPCLEANBUFFERS
GO

DBCC PROCCACHE WITH NO_INFOMSGS;

DBCC SHOWCONTIG ('HumanResources.Employee');
GO
DBCC SHOWCONTIG (@id, @indid);
GO
DBCC SHOWCONTIG ('Production.Product', 1) WITH FAST;
GO
DBCC SHOWCONTIG WITH TABLERESULTS, ALL_INDEXES;
GO

DBCC SHRINKLOG ( SIZE = 100 MB );
DBCC SHRINKLOG ( SIZE = DEFAULT );
DBCC SHRINKLOG;

-- Uses AdventureWorks2022
DBCC PDW_SHOWSPACEUSED ( "AdventureWorksPDW2012.dbo.FactInternetSales" );
DBCC PDW_SHOWSPACEUSED ( "AdventureWorksPDW2012..FactInternetSales" );
DBCC PDW_SHOWSPACEUSED ( "dbo.FactInternetSales" );
DBCC PDW_SHOWSPACEUSED ( FactInternetSales );
-- Uses AdventureWorks2022
DBCC PDW_SHOWSPACEUSED;
