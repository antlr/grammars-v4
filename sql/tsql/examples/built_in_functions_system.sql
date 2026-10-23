USE AdventureWorks2012;
GO
CREATE TABLE myTable (column1 INT, column2 VARCHAR(256));
GO
INSERT INTO myTable VALUES (1, 'test');
GO
SELECT BINARY_CHECKSUM(*) from myTable;
GO
UPDATE myTable set column2 = 'TEST';
GO
SELECT BINARY_CHECKSUM(*) from myTable;
GO

-- Create a checksum index.

SET ARITHABORT ON;
USE AdventureWorks2012;
GO
ALTER TABLE Production.Product
ADD cs_Pname AS CHECKSUM(Name);
GO
CREATE INDEX Pname_index ON Production.Product (cs_Pname);
GO

/*Use the index in a SELECT query. Add a second search
condition to catch stray cases where checksums match,
but the values are not the same.*/

SELECT *
FROM Production.Product
WHERE CHECKSUM(N'Bearing Ball') = cs_Pname
AND Name = N'Bearing Ball';
GO
INSERT INTO player (name, surname, info )
VALUES (N'Ovidiu', N'Cracium',
        COMPRESS(N'{"sport":"Tennis","age": 28,"rank":1,"points":15258, turn":17}'));

INSERT INTO player (name, surname, info )
VALUES (N'Michael', N'Raheem', compress(@info));
DELETE FROM player
OUTPUT deleted.id, deleted.name, deleted.surname, deleted.datemodifier, COMPRESS(deleted.info)
INTO dbo.inactivePlayers
WHERE datemodified < @startOfYear;

SELECT
ConnectionProperty('net_transport') AS 'Net transport',
ConnectionProperty('protocol_type') AS 'Protocol type';

SET CONTEXT_INFO 0x1256698456;
GO
SELECT CONTEXT_INFO();
GO

SELECT CURRENT_REQUEST_ID();

SELECT CURRENT_TRANSACTION_ID();

SELECT _id, name, surname, datemodified,
             CAST(DECOMPRESS(info) AS NVARCHAR(MAX)) AS info
FROM player;

CREATE TABLE example_table (
    _id INT PRIMARY KEY IDENTITY,
    name NVARCHAR(max),
    surname NVARCHAR(max),
    info VARBINARY(max),
    info_json as CAST(DECOMPRESS(info) as NVARCHAR(max))
);

BEGIN TRY
    -- Generate a divide-by-zero error.
    SELECT 1/0;
END TRY
BEGIN CATCH
    SELECT ERROR_LINE() AS ErrorLine;
END CATCH;
GO

-- Verify that the stored procedure does not already exist.
IF OBJECT_ID ( 'usp_ExampleProc', 'P' ) IS NOT NULL
    DROP PROCEDURE usp_ExampleProc;
GO

-- Create a stored procedure that
-- generates a divide-by-zero error.
CREATE PROCEDURE usp_ExampleProc
AS
    SELECT 1/0;
GO

BEGIN TRY
    -- Execute the stored procedure inside the TRY block.
    EXECUTE usp_ExampleProc;
END TRY
BEGIN CATCH
    SELECT ERROR_LINE() AS ErrorLine;
END CATCH;
GO

BEGIN TRY
    -- Generate a divide-by-zero error.
    SELECT 1/0;
END TRY
BEGIN CATCH
    SELECT
        ERROR_NUMBER() AS ErrorNumber,
        ERROR_SEVERITY() AS ErrorSeverity,
        ERROR_STATE() AS ErrorState,
        ERROR_PROCEDURE() AS ErrorProcedure,
        ERROR_LINE() AS ErrorLine,
        ERROR_MESSAGE() AS ErrorMessage;
END CATCH;
GO

BEGIN TRY
    -- Generate a divide-by-zero error.
    SELECT 1/0;
END TRY
BEGIN CATCH
    SELECT ERROR_MESSAGE() AS ErrorMessage;
END CATCH;
GO

BEGIN TRY
    -- Generate a divide-by-zero error.
    SELECT 1/0;
END TRY
BEGIN CATCH
    SELECT
        ERROR_NUMBER() AS ErrorNumber
        ,ERROR_SEVERITY() AS ErrorSeverity
        ,ERROR_STATE() AS ErrorState
        ,ERROR_PROCEDURE() AS ErrorProcedure
        ,ERROR_LINE() AS ErrorLine
        ,ERROR_MESSAGE() AS ErrorMessage;
END CATCH;
GO

-- Verify that the stored procedure does not already exist.
IF OBJECT_ID ( 'usp_ExampleProc', 'P' ) IS NOT NULL
    DROP PROCEDURE usp_ExampleProc;
GO

-- Create a stored procedure that
-- generates a divide-by-zero error.
CREATE PROCEDURE usp_ExampleProc
AS
    SELECT 1/0;
GO

BEGIN TRY
    -- Execute the stored procedure inside the TRY block.
    EXECUTE usp_ExampleProc;
END TRY
BEGIN CATCH
    SELECT ERROR_PROCEDURE() AS ErrorProcedure;
END CATCH;
GO

-- Verify that the stored procedure does not already exist.
IF OBJECT_ID ( 'usp_ExampleProc', 'P' ) IS NOT NULL
    DROP PROCEDURE usp_ExampleProc;
GO

-- Create a stored procedure that
-- generates a divide-by-zero error.
CREATE PROCEDURE usp_ExampleProc
AS
    SELECT 1/0;
GO

BEGIN TRY
    -- Execute the stored procedure inside the TRY block.
    EXECUTE usp_ExampleProc;
END TRY
BEGIN CATCH
    SELECT
        ERROR_NUMBER() AS ErrorNumber,
        ERROR_SEVERITY() AS ErrorSeverity,
        ERROR_STATE() AS ErrorState,
        ERROR_PROCEDURE() AS ErrorProcedure,
        ERROR_MESSAGE() AS ErrorMessage,
        ERROR_LINE() AS ErrorLine;
        END CATCH;
GO

SELECT text FROM sys.messages WHERE message_id = 20009 AND language_id = 1033;
DECLARE @var1 VARCHAR(200);
SELECT @var1 = FORMATMESSAGE(20009, 'First Variable', 'Second Variable');
SELECT @var1;

SELECT FORMATMESSAGE('This is the %s and this is the %s.', 'first variable', 'second variable') AS Result;

SELECT FORMATMESSAGE('Signed int %i, %d %i, %d, %+i, %+d, %+i, %+d', 5, -5, 50, -50, -11, -11, 11, 11);
SELECT FORMATMESSAGE('Signed int with up to 3 leading zeros %03i', 5);
SELECT FORMATMESSAGE('Signed int with up to 20 leading zeros %020i', 5);
SELECT FORMATMESSAGE('Signed int with leading zero 0 %020i', -55);
SELECT FORMATMESSAGE('Bigint %I64d', 3000000000);
SELECT FORMATMESSAGE('Unsigned int %u, %u', 50, -50);
SELECT FORMATMESSAGE('Unsigned octal %o, %o', 50, -50);
SELECT FORMATMESSAGE('Unsigned hexadecimal %x, %X, %X, %X, %x', 11, 11, -11, 50, -50);
SELECT FORMATMESSAGE('Unsigned octal with prefix: %#o, %#o', 50, -50);
SELECT FORMATMESSAGE('Unsigned hexadecimal with prefix: %#x, %#X, %#X, %X, %x', 11, 11, -11, 50, -50);
SELECT FORMATMESSAGE('Hello %s!', 'TEST');
SELECT FORMATMESSAGE('Hello %20s!', 'TEST');
SELECT FORMATMESSAGE('Hello %-20s!', 'TEST');

SELECT GET_FILESTREAM_TRANSACTION_CONTEXT();

USE AdventureWorks2012;
GO
SELECT GETANSINULL('AdventureWorks2012')
GO

CREATE TABLE Orders
   (OrderID     INT       PRIMARY KEY,
    CustomerID  NCHAR(5)  REFERENCES Customers(CustomerID),
    TerminalID  CHAR(8)   NOT NULL DEFAULT HOST_ID(),
    OrderDate   DATETIME  NOT NULL,
    ShipDate    DATETIME  NULL,
    ShipperID   INT       NULL REFERENCES Shippers(ShipperID));
GO

CREATE TABLE Orders
   (OrderID     INT        PRIMARY KEY,
    CustomerID  NCHAR(5)   REFERENCES Customers(CustomerID),
    Workstation NCHAR(30)  NOT NULL DEFAULT HOST_NAME(),
    OrderDate   DATETIME   NOT NULL,
    ShipDate    DATETIME   NULL,
    ShipperID   INT        NULL REFERENCES Shippers(ShipperID));
GO

USE AdventureWorks2012;
GO
SELECT AVG(ISNULL(Weight, 50))
FROM Production.Product;
GO

USE AdventureWorks2012;
GO
SELECT Description, DiscountPct, MinQty, ISNULL(MaxQty, 0.00) AS 'Max Quantity'
FROM Sales.SpecialOffer;
GO

USE AdventureWorks2012;
GO
SELECT Name, Weight
FROM Production.Product
WHERE Weight IS NULL;
GO

-- Uses AdventureWorks

SELECT AVG(ISNULL(Weight, 50))
FROM dbo.DimProduct;

-- Uses AdventureWorks

SELECT ResellerName,
       ISNULL(MinPaymentAmount,0) AS MinimumPayment
FROM dbo.DimReseller
ORDER BY ResellerName;

-- Uses AdventureWorks

SELECT EnglishProductName, Weight
FROM dbo.DimProduct
WHERE Weight IS NULL;

USE AdventureWorks2012;
GO
SELECT City, PostalCode
FROM Person.Address
WHERE ISNUMERIC(PostalCode) <> 1;
GO

USE master;
GO
SELECT name, ISNUMERIC(name) AS IsNameANumber, database_id, ISNUMERIC(database_id) AS IsIdANumber
FROM sys.databases;
GO

-- Create a table that has a ROWVERSION column in it.
CREATE TABLE RowVersionTestTable (rv ROWVERSION)
GO

-- Print the current values for the database.
PRINT ''
PRINT 'DBTS'
PRINT @@DBTS
PRINT 'MIN_ACTIVE_ROWVERSION'
PRINT MIN_ACTIVE_ROWVERSION()
GO
---------------- Results ----------------
--DBTS
--0x00000000000007E2
--MIN_ACTIVE_ROWVERSION
--0x00000000000007E3

-- Insert a row.
INSERT INTO RowVersionTestTable VALUES (DEFAULT)
SELECT * FROM RowVersionTestTable
GO
---------------- Results ----------------
--rv
--0x00000000000007E3

-- Print the current values for the database.
PRINT ''
PRINT 'DBTS'
PRINT @@DBTS
PRINT 'MIN_ACTIVE_ROWVERSION'
PRINT MIN_ACTIVE_ROWVERSION()
GO
---------------- Results ----------------
--DBTS
--0x00000000000007E3
--MIN_ACTIVE_ROWVERSION
--0x00000000000007E4

-- Insert a new row inside a transaction but do not commit.
BEGIN TRAN
INSERT INTO RowVersionTestTable VALUES (DEFAULT)
SELECT * FROM RowVersionTestTable
GO
---------------- Results ----------------
--rv
--0x00000000000007E3
--0x00000000000007E4

-- Print the current values for the database.
PRINT ''
PRINT 'DBTS'
PRINT @@DBTS
PRINT 'MIN_ACTIVE_ROWVERSION'
PRINT MIN_ACTIVE_ROWVERSION()
GO
---------------- Results ----------------
--DBTS
--0x00000000000007E4
--MIN_ACTIVE_ROWVERSION
--0x00000000000007E4

-- Commit the transaction.
COMMIT
GO

-- Print the current values for the database.
PRINT ''
PRINT 'DBTS'
PRINT @@DBTS
PRINT 'MIN_ACTIVE_ROWVERSION'
PRINT MIN_ACTIVE_ROWVERSION()
GO
---------------- Results ----------------
--DBTS
--0x00000000000007E4
--MIN_ACTIVE_ROWVERSION
--0x00000000000007E5

-- Creating a local variable with DECLARE/SET syntax.
DECLARE @myid uniqueidentifier
SET @myid = NEWID()
PRINT 'Value of @myid is: '+ CONVERT(varchar(255), @myid)

-- Creating a table using NEWID for uniqueidentifier data type.
CREATE TABLE cust
(
 CustomerID uniqueidentifier NOT NULL
   DEFAULT newid(),
 Company VARCHAR(30) NOT NULL,
 ContactName VARCHAR(60) NOT NULL,
 Address VARCHAR(30) NOT NULL,
 City VARCHAR(30) NOT NULL,
 StateProvince VARCHAR(10) NULL,
 PostalCode VARCHAR(10) NOT NULL,
 CountryRegion VARCHAR(20) NOT NULL,
 Telephone VARCHAR(15) NOT NULL,
 Fax VARCHAR(15) NULL
);
GO
-- Inserting 5 rows into cust table.
INSERT cust
(Company, ContactName, Address, City, StateProvince,
 PostalCode, CountryRegion, Telephone, Fax)
VALUES
 ('Wartian Herkku', 'Pirkko Koskitalo', 'Torikatu 38', 'Oulu', NULL,
 '90110', 'Finland', '981-443655', '981-443655')
,('Wellington Importadora', 'Paula Parente', 'Rua do Mercado, 12', 'Resende', 'SP',
 '08737-363', 'Brasil', '(14) 555-8122', '')
,('Cactus Comidas para Ilevar', 'Patricio Simpson', 'Cerrito 333', 'Buenos Aires', NULL,
 '1010', 'Argentina', '(1) 135-5555', '(1) 135-4892')
,('Ernst Handel', 'Roland Mendel', 'Kirchgasse 6', 'Graz', NULL,
 '8010', 'Austria', '7675-3425', '7675-3426')
,('Maison Dewey', 'Catherine Dewey', 'Rue Joseph-Bens 532', 'Bruxelles', NULL,
 'B-1180', 'Belgium', '(02) 201 24 67', '(02) 201 24 68');
GO

DECLARE @myid uniqueidentifier ;
SET @myid = 'A972C577-DFB0-064E-1189-0154C99310DAAC12';
SELECT @myid;
GO

CREATE TABLE myTable (ColumnA uniqueidentifier DEFAULT NEWSEQUENTIALID());

CREATE TABLE myTable (ColumnA uniqueidentifier DEFAULT dbo.myfunction(NEWSEQUENTIALID()));

EXEC sp_set_session_context 'user_id', 4;
SELECT SESSION_CONTEXT(N'user_id');

USE AdventureWorks2012;
GO

-- SET XACT_ABORT ON will render the transaction uncommittable
-- when the constraint violation occurs.
SET XACT_ABORT ON;

BEGIN TRY
    BEGIN TRANSACTION;
        -- A FOREIGN KEY constraint exists on this table. This
        -- statement will generate a constraint violation error.
        DELETE FROM Production.Product
            WHERE ProductID = 980;

    -- If the delete operation succeeds, commit the transaction. The CATCH
    -- block will not execute.
    COMMIT TRANSACTION;
END TRY
BEGIN CATCH
    -- Test XACT_STATE for 0, 1, or -1.
    -- If 1, the transaction is committable.
    -- If -1, the transaction is uncommittable and should
    --     be rolled back.
    -- XACT_STATE = 0 means there is no transaction and
    --     a commit or rollback operation would generate an error.

    -- Test whether the transaction is uncommittable.
    IF (XACT_STATE()) = -1
    BEGIN
        PRINT 'The transaction is in an uncommittable state.' +
              ' Rolling back transaction.'
        ROLLBACK TRANSACTION;
    END;

    -- Test whether the transaction is active and valid.
    IF (XACT_STATE()) = 1
    BEGIN
        PRINT 'The transaction is committable.' +
              ' Committing transaction.'
        COMMIT TRANSACTION;
    END;
END CATCH;
GO
