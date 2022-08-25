UPDATE [dbo].[FactResellerSalesXL_CCI] WITH (ROWLOCK)
SET UnitPrice = 50
WHERE ProductKey = 150;

SELECT *
FROM dbo.Customers AS c WITH (SNAPSHOT)
LEFT JOIN dbo.[Order History] AS oh
    ON c.customer_id=oh.customer_id;

IF EXISTS (SELECT name FROM sys.indexes
    WHERE name = N'FIBillOfMaterialsWithComponentID'
    AND object_id = OBJECT_ID(N'Production.BillOfMaterials'))
DROP INDEX FIBillOfMaterialsWithComponentID
    ON Production.BillOfMaterials;
GO
CREATE NONCLUSTERED INDEX "FIBillOfMaterialsWithComponentID"
    ON Production.BillOfMaterials (ComponentID, StartDate, EndDate)
    WHERE ComponentID IN (533, 324, 753);
GO
SELECT StartDate, ComponentID FROM Production.BillOfMaterials
    WITH( INDEX (FIBillOfMaterialsWithComponentID) )
    WHERE ComponentID in (533, 324, 753, 855, 924);
GO

UPDATE Production.Product
WITH (TABLOCK)
SET ListPrice = ListPrice * 1.10
WHERE ProductNumber LIKE 'BK-%';
GO

SELECT *
FROM Sales.SalesOrderHeader AS h
INNER JOIN Sales.SalesOrderDetail AS d WITH (FORCESEEK)
    ON h.SalesOrderID = d.SalesOrderID
WHERE h.TotalDue > 100
AND (d.OrderQty > 5 OR d.LineTotal < 1000.00);
GO

SELECT h.SalesOrderID, h.TotalDue, d.OrderQty
FROM Sales.SalesOrderHeader AS h
    INNER JOIN Sales.SalesOrderDetail AS d
    WITH (FORCESEEK (PK_SalesOrderDetail_SalesOrderID_SalesOrderDetailID (SalesOrderID)))
    ON h.SalesOrderID = d.SalesOrderID
WHERE h.TotalDue > 100
AND (d.OrderQty > 5 OR d.LineTotal < 1000.00);
GO

SELECT h.SalesOrderID, h.TotalDue, d.OrderQty
FROM Sales.SalesOrderHeader AS h
    INNER JOIN Sales.SalesOrderDetail AS d
    WITH (FORCESCAN)
    ON h.SalesOrderID = d.SalesOrderID
WHERE h.TotalDue > 100
AND (d.OrderQty > 5 OR d.LineTotal < 1000.00);

--

-- This is no hint but an alias
update alias set x = 1 from t alias

-- These are hints on tables, not function calls
select * from dbo.t (nolock) t join dbo.u (nolock) on t.x = u.x
select * from dbo.t (nolock) ta join dbo.u (nolock) ua on t.x = ua.x
select * from t (nolock) ta
select * from t ta (nolock)
