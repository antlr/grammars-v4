USE AdventureWorks2012;  
GO  
ALTER SCHEMA HumanResources TRANSFER Person.Address;  
GO  
ALTER SCHEMA Person TRANSFER type::Production.TestType ;
GO
ALTER SCHEMA Sales TRANSFER OBJECT::dbo.Region;
GO
