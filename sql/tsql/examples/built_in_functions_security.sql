CREATE DATABASE TEST1;
GO
USE TEST1
CREATE CERTIFICATE Shipping04
ENCRYPTION BY PASSWORD = 'pGFD4bb925DGvbd2439587y'
WITH SUBJECT = 'Sammamish Shipping Records',
EXPIRY_DATE = '20401031';
GO
SELECT CERTENCODED(CERT_ID('Shipping04'));
GO

CREATE DATABASE TEST1;
GO
USE TEST1
CREATE MASTER KEY ENCRYPTION BY PASSWORD = 'Use 5tr0ng P^55Words'
GO
CREATE CERTIFICATE Shipping04
WITH SUBJECT = 'Sammamish Shipping Records',
EXPIRY_DATE = '20401031';
GO
SELECT CERTPRIVATEKEY(CERT_ID('Shipping04'), 'jklalkaa/; uia3dd');
GO

SELECT CURRENT_USER;
GO

USE AdventureWorks2012;
GO
IF EXISTS (SELECT TABLE_NAME FROM INFORMATION_SCHEMA.TABLES
      WHERE TABLE_NAME = 'orders22')
   DROP TABLE orders22;
GO
SET NOCOUNT ON;
CREATE TABLE orders22
(
order_id int IDENTITY(1000, 1) NOT NULL,
cust_id  int NOT NULL,
order_date smalldatetime NOT NULL DEFAULT GETDATE(),
order_amt money NOT NULL,
order_person char(30) NOT NULL DEFAULT CURRENT_USER
);
GO

SELECT CURRENT_USER;
GO
EXECUTE AS USER = 'Arnalfo';
GO
SELECT CURRENT_USER;
GO
REVERT;
GO
SELECT CURRENT_USER;
GO
EXECUTE AS CALLER
GO
SELECT CURRENT_USER;
GO

SELECT DATABASE_PRINCIPAL_ID();
GO

SELECT DATABASE_PRINCIPAL_ID('db_owner');
GO

SELECT HAS_DBACCESS('AdventureWorks2012');
GO

SELECT HAS_DBACCESS('AdventureWorksPDW2012');
GO

SELECT HAS_PERMS_BY_NAME(null, null, 'VIEW SERVER STATE');

SELECT HAS_PERMS_BY_NAME('Ps', 'LOGIN', 'IMPERSONATE');

SELECT HAS_PERMS_BY_NAME(db_name(), 'DATABASE', 'ANY');

EXECUTE AS user = 'Pd'
GO
SELECT HAS_PERMS_BY_NAME(db_name(), 'DATABASE', 'ANY');
GO
REVERT;
GO

SELECT HAS_PERMS_BY_NAME(db_name(), 'DATABASE', 'CREATE PROCEDURE')
    & HAS_PERMS_BY_NAME('S', 'SCHEMA', 'ALTER') AS _can_create_procs,
    HAS_PERMS_BY_NAME(db_name(), 'DATABASE', 'CREATE TABLE') &
    HAS_PERMS_BY_NAME('S', 'SCHEMA', 'ALTER') AS _can_create_tables;

SELECT HAS_PERMS_BY_NAME
(QUOTENAME(SCHEMA_NAME(schema_id)) + '.' + QUOTENAME(name),
    'OBJECT', 'SELECT') AS have_select, * FROM sys.tables

SELECT HAS_PERMS_BY_NAME('Sales.SalesPerson', 'OBJECT', 'INSERT');

SELECT HAS_PERMS_BY_NAME('AdventureWorks2012.Sales.SalesPerson',
    'OBJECT', 'INSERT');

SELECT name AS column_name,
    HAS_PERMS_BY_NAME('T', 'OBJECT', 'SELECT', name, 'COLUMN')
    AS can_select
    FROM sys.columns AS c
    WHERE c.object_id=object_id('T');

-- Test membership in db_owner and print appropriate message.
IF IS_MEMBER ('db_owner') = 1
   PRINT 'Current user is a member of the db_owner role'
ELSE IF IS_MEMBER ('db_owner') = 0
   PRINT 'Current user is NOT a member of the db_owner role'
ELSE IF IS_MEMBER ('db_owner') IS NULL
   PRINT 'ERROR: Invalid group / role specified';
GO

-- Execute SELECT if user is a member of ADVWORKS\Shipping.
IF IS_MEMBER ('ADVWORKS\Shipping') = 1
   SELECT 'User ' + USER + ' is a member of ADVWORKS\Shipping.';
GO

IF IS_ROLEMEMBER ('db_datareader') = 1
   print 'Current user is a member of the db_datareader role'
ELSE IF IS_ROLEMEMBER ('db_datareader') = 0
   print 'Current user is NOT a member of the db_datareader role'
ELSE IF IS_ROLEMEMBER ('db_datareader') IS NULL
   print 'ERROR: The database role specified is not valid.';

IF IS_SRVROLEMEMBER ('sysadmin') = 1
   print 'Current user''s login is a member of the sysadmin role'
ELSE IF IS_SRVROLEMEMBER ('sysadmin') = 0
   print 'Current user''s login is NOT a member of the sysadmin role'
ELSE IF IS_SRVROLEMEMBER ('sysadmin') IS NULL
   print 'ERROR: The server role specified is not valid.';

SELECT IS_SRVROLEMEMBER('diskadmin', 'Contoso\Pat');

SELECT LOGINPROPERTY('John3', 'IsMustChange');
GO

SELECT LOGINPROPERTY('John3', 'IsLocked');
GO

USE AdventureWorks2012;
GO
--Create a temporary login and user.
CREATE LOGIN login1 WITH PASSWORD = 'J345#$)thb';
CREATE USER user1 FOR LOGIN login1;
GO
--Execute a context switch to the temporary login account.
DECLARE @original_login sysname;
DECLARE @current_context sysname;
EXECUTE AS LOGIN = 'login1';
SET @original_login = ORIGINAL_LOGIN();
SET @current_context = SUSER_SNAME();
SELECT 'The current executing context is: '+ @current_context;
SELECT 'The original login in this session was: '+ @original_login
GO
-- Return to the original execution context
-- and remove the temporary principal.
REVERT;
GO
DROP LOGIN login1;
DROP USER user1;
GO

IF PERMISSIONS()&2=2
   CREATE TABLE test_table (col1 INT)
ELSE
   PRINT 'ERROR: The current user cannot create a table.';

IF PERMISSIONS(OBJECT_ID('AdventureWorks2012.Person.Address','U'))&8=8
   PRINT 'The current user can insert data into Person.Address.'
ELSE
   PRINT 'ERROR: The current user cannot insert data into Person.Address.';

IF PERMISSIONS(OBJECT_ID('AdventureWorks2012.Person.Address','U'))&0x80000=0x80000
   PRINT 'INSERT on Person.Address is grantable.'
ELSE
   PRINT 'You may not GRANT INSERT permissions on Person.Address.';

SELECT PWDENCRYPT('password');

SELECT name FROM sys.sql_logins
WHERE PWDCOMPARE('', password_hash) = 1 ;

SELECT name FROM sys.sql_logins
WHERE PWDCOMPARE('password', password_hash) = 1 ;

DECLARE @session_usr NCHAR(30);
SET @session_usr = SESSION_USER;
SELECT 'This session''s current user is: '+ @session_usr;
GO

USE AdventureWorks2012;
GO
CREATE TABLE deliveries3
(
 order_id INT IDENTITY(5000, 1) NOT NULL,
 cust_id  INT NOT NULL,
 order_date SMALLDATETIME NOT NULL DEFAULT GETDATE(),
 delivery_date SMALLDATETIME NOT NULL DEFAULT
    DATEADD(dd, 10, GETDATE()),
 received_shipment NCHAR(30) NOT NULL DEFAULT SESSION_USER
);
GO

EXECUTE AS USER = 'Wanida'
INSERT deliveries3 (cust_id)
VALUES (7510);
INSERT deliveries3 (cust_id)
VALUES (7231);
REVERT;
EXECUTE AS USER = 'Sylvester'
INSERT deliveries3 (cust_id)
VALUES (7028);
REVERT;
EXECUTE AS USER = 'Alejandro'
INSERT deliveries3 (cust_id)
VALUES (7392);
INSERT deliveries3 (cust_id)
VALUES (7452);
REVERT;
GO

SELECT order_id AS 'Order #', cust_id AS 'Customer #',
   delivery_date AS 'When Delivered', received_shipment
   AS 'Received By'
FROM deliveries3
ORDER BY order_id;
GO

SELECT SESSION_USER;

SELECT   SESSIONPROPERTY ('CONCAT_NULL_YIELDS_NULL')

SELECT SUSER_ID('sa');

SELECT SUSER_NAME(1);

SELECT SUSER_SID();

SELECT SUSER_SID('sa');
GO

SELECT SUSER_SID('London\Workstation1');
GO

USE AdventureWorks2012;
GO
CREATE TABLE sid_example
(
login_sid   VARBINARY(85) DEFAULT SUSER_SID(),
login_name  VARCHAR(30) DEFAULT SYSTEM_USER,
login_dept  VARCHAR(10) DEFAULT 'SALES',
login_date  DATETIME DEFAULT GETDATE()
);
GO
INSERT sid_example DEFAULT VALUES;
GO

SELECT SUSER_SNAME(SUSER_SID('TestComputer\User', 0));

SELECT SUSER_SNAME();
GO

SELECT SUSER_SNAME(0x010500000000000515000000a065cf7e784b9b5fe77c87705a2e0000);
GO

USE AdventureWorks2019;
GO

CREATE TABLE sname_example (
    login_sname SYSNAME DEFAULT SUSER_SNAME(),
    employee_id UNIQUEIDENTIFIER DEFAULT NEWID(),
    login_date DATETIME DEFAULT GETDATE()
    );
GO

INSERT sname_example DEFAULT
VALUES;
GO

SELECT SUSER_SNAME();
GO

EXECUTE AS LOGIN = 'WanidaBenShoof';
SELECT SUSER_SNAME();

REVERT;
GO

SELECT SUSER_SNAME();
GO

SELECT SUSER_SNAME(0x01);
GO

SELECT SUSER_SNAME() AS CurrentLogin;
GO

DECLARE @sys_usr CHAR(30);
SET @sys_usr = SYSTEM_USER;
SELECT 'The current system user is: '+ @sys_usr;
GO

USE AdventureWorks2012;
GO
CREATE TABLE Sales.Sales_Tracking
(
    Territory_id INT IDENTITY(2000, 1) NOT NULL,
    Rep_id INT NOT NULL,
    Last_sale DATETIME NOT NULL DEFAULT GETDATE(),
    SRep_tracking_user VARCHAR(30) NOT NULL DEFAULT SYSTEM_USER
);
GO
INSERT Sales.Sales_Tracking (Rep_id)
VALUES (151);
INSERT Sales.Sales_Tracking (Rep_id, Last_sale)
VALUES (293, '19980515');
INSERT Sales.Sales_Tracking (Rep_id, Last_sale)
VALUES (27882, '19980620');
INSERT Sales.Sales_Tracking (Rep_id)
VALUES (21392);
INSERT Sales.Sales_Tracking (Rep_id, Last_sale)
VALUES (24283, '19981130');
GO

SELECT SYSTEM_USER;

DECLARE @usr CHAR(30)
SET @usr = user
SELECT 'The current user''s database username is: '+ @usr
GO

USE AdventureWorks2012;
GO
CREATE TABLE inventory22
(
 part_id INT IDENTITY(100, 1) NOT NULL,
 description VARCHAR(30) NOT NULL,
 entry_person VARCHAR(30) NOT NULL DEFAULT USER
)
GO
INSERT inventory22 (description)
VALUES ('Red pencil')
INSERT inventory22 (description)
VALUES ('Blue pencil')
INSERT inventory22 (description)
VALUES ('Green pencil')
INSERT inventory22 (description)
VALUES ('Black pencil')
INSERT inventory22 (description)
VALUES ('Yellow pencil')
GO

SELECT USER;
GO
EXECUTE AS USER = 'Mario';
GO
SELECT USER;
GO
REVERT;
GO
SELECT USER;
GO

USE AdventureWorks2012;
SELECT USER_ID('Harold');
GO

SELECT USER_NAME(13);
GO

SELECT name FROM sysusers WHERE name = USER_NAME(1);
GO

SELECT USER_NAME();
GO
EXECUTE AS USER = 'Zelig';
GO
SELECT USER_NAME();
GO
REVERT;
GO
SELECT USER_NAME();
GO
