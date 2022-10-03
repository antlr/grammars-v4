UPDATE STATISTICS t1 (a,b);

UPDATE STATISTICS t1 (a) WITH SAMPLE 10 ROWS;

UPDATE STATISTICS t1 (a) WITH NORECOMPUTE;

UPDATE STATISTICS t1 (a) WITH INCREMENTAL = ON;

UPDATE STATISTICS t1 (a) WITH stats_stream = 0x01;

USE AdventureWorks2012;
GO
UPDATE STATISTICS Sales.SalesOrderDetail;
GO

USE AdventureWorks2012;
GO
UPDATE STATISTICS Sales.SalesOrderDetail AK_SalesOrderDetail_rowguid;
GO

USE AdventureWorks2012;
GO
CREATE STATISTICS Products
    ON Production.Product ([Name], ProductNumber)
    WITH SAMPLE 50 PERCENT
-- Time passes. The UPDATE STATISTICS statement is then executed.
UPDATE STATISTICS Production.Product(Products)
    WITH SAMPLE 50 PERCENT;

USE AdventureWorks2012;
GO
UPDATE STATISTICS Production.Product(Products)
    WITH FULLSCAN, NORECOMPUTE;
GO

UPDATE STATISTICS Customer (CustomerStats1);

UPDATE STATISTICS Customer (CustomerStats1) WITH FULLSCAN;

UPDATE STATISTICS Customer;

UPDATE STATISTICS Customer (CustomerStats1) WITH AUTO_DROP = ON
