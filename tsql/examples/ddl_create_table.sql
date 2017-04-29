-- https://docs.microsoft.com/en-us/sql/t-sql/statements/create-table-transact-sql
--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- create table with defining foreign key when define column
CREATE TABLE dbo.Employee 
(
	EmployeeID int PRIMARY KEY CLUSTERED, 
	SalesPersonID int NULL REFERENCES SalesPerson(SalesPersonID)
);  
--+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
-- create table and add separate constraint which is foreign key
CREATE TABLE dbo.Employee 
(
	EmployeeID int PRIMARY KEY CLUSTERED, 
	ProductID int, 
	SpecialOfferID int, 
	CONSTRAINT FK_SpecialOfferProduct_SalesOrderDetail FOREIGN KEY (ProductID, SpecialOfferID) REFERENCES SpecialOfferProduct (ProductID, SpecialOfferID)
);