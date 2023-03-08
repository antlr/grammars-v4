CREATE TRIGGER reminder1  
ON Sales.Customer  
AFTER INSERT, UPDATE   
AS RAISERROR ('Notify Customer Relations', 16, 10);  
GO

CREATE OR ALTER TRIGGER reminder2
ON Sales.Customer  
AFTER INSERT, UPDATE, DELETE   
AS  
   EXEC msdb.dbo.sp_send_dbmail  
        @profile_name = 'AdventureWorks2012 Administrator',  
        @recipients = 'danw@Adventure-Works.com',  
        @body = 'Don''t forget to print a report for the sales force.',  
        @subject = 'Reminder';  
GO  

CREATE TRIGGER Purchasing.LowCredit ON Purchasing.PurchaseOrderHeader  
AFTER INSERT  
AS  
IF EXISTS (SELECT *  
           FROM Purchasing.PurchaseOrderHeader AS p   
           JOIN inserted AS i   
           ON p.PurchaseOrderID = i.PurchaseOrderID   
           JOIN Purchasing.Vendor AS v   
           ON v.BusinessEntityID = p.VendorID  
           WHERE v.CreditRating = 5  
          )  
BEGIN  
RAISERROR ('A vendor''s credit rating is too low to accept new  
purchase orders.', 16, 1);  
ROLLBACK TRANSACTION;  
RETURN   
END;  
GO  

CREATE TRIGGER safety   
ON DATABASE   
FOR DROP_SYNONYM  
AS   
   RAISERROR ('You must disable Trigger "safety" to drop synonyms!',10, 1)  
   ROLLBACK  
GO  
DROP TRIGGER safety  
ON DATABASE;  
GO  

CREATE TRIGGER triggerOnDatabase
ON DATABASE
FOR create_procedure, alter_procedure, drop_procedure,
    create_table, alter_table, drop_table,
    create_trigger, alter_trigger, drop_trigger,
    create_view, alter_view, drop_view,
    create_function, alter_function, drop_function,
    create_index, alter_index, drop_index
AS
BEGIN
    declare @variable int        
END
GO

DROP TRIGGER triggerOnDatabase  
ON DATABASE;  
GO  

CREATE TRIGGER ddl_trig_database   
ON ALL SERVER   
FOR CREATE_DATABASE   
AS   
    PRINT 'Database Created.'  
    SELECT EVENTDATA().value('(/EVENT_INSTANCE/TSQLCommand/CommandText)[1]','nvarchar(max)')  
GO  
DROP TRIGGER ddl_trig_database  
ON ALL SERVER;  
GO  
DISABLE TRIGGER Person.uAddress ON Person.Address;
GO
ENABLE Trigger Person.uAddress ON Person.Address;
DISABLE TRIGGER safety ON DATABASE;
GO
ENABLE TRIGGER safety ON DATABASE;
ENABLE Trigger ALL ON ALL SERVER;
GO
DISABLE TRIGGER Person.uAddress ON Person.Address;
GO
DISABLE TRIGGER safety ON DATABASE;
GO
DISABLE Trigger ALL ON ALL SERVER;
GO


CREATE OR REPLACE TRIGGER triggerOnDatabase
ON DATABASE
FOR create_procedure
AS
BEGIN
    declare @variable int
END
GO

CREATE OR REPLACE TRIGGER myTrigger ON myTable
FOR UPDATE
AS
   PRINT 'This is the trigger from create-or-replace'
GO