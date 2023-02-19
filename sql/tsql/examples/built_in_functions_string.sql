SELECT ASCII('A') AS A, ASCII('B') AS B,
ASCII('a') AS a, ASCII('b') AS b,
ASCII(1) AS [1], ASCII(2) AS [2];

SELECT ASCII('P') AS [ASCII], ASCII('æ') AS [Extended_ASCII];

SELECT NCHAR(80) AS [CHARACTER], NCHAR(195) AS [CHARACTER];

SELECT UNICODE('æ') AS [Extended_ASCII], NCHAR(230) AS [CHARACTER];

SET TEXTSIZE 0;
-- Create variables for the character string and for the current
-- position in the string.
DECLARE @position INT, @string CHAR(8);
-- Initialize the current position and the string variables.
SET @position = 1;
SET @string = 'New Moon';
WHILE @position <= DATALENGTH(@string)
   BEGIN
   SELECT ASCII(SUBSTRING(@string, @position, 1)),
      CHAR(ASCII(SUBSTRING(@string, @position, 1)))
   SET @position = @position + 1
   END;
GO

SELECT p.FirstName + ' ' + p.LastName, + CHAR(13)  + pe.EmailAddress
FROM Person.Person p
INNER JOIN Person.EmailAddress pe ON p.BusinessEntityID = pe.BusinessEntityID
  AND p.BusinessEntityID = 1;
GO

SELECT CHAR(65) AS [65], CHAR(66) AS [66],
CHAR(97) AS [97], CHAR(98) AS [98],
CHAR(49) AS [49], CHAR(50) AS [50];

SELECT name, 'was created on ', create_date, CHAR(13), name, 'is currently ', state_desc
FROM sys.databases;
GO

SELECT CHAR(188) AS single_byte_representing_complete_character,
  CHAR(0xBC) AS single_byte_representing_complete_character;
GO

SELECT CHAR(129) AS first_byte_of_double_byte_character,
  CHAR(0x81) AS first_byte_of_double_byte_character;
GO

CREATE DATABASE [multibyte-char-context]
  COLLATE Japanese_CI_AI
GO
USE [multibyte-char-context]
GO
SELECT NCHAR(0x266A) AS [eighth-note]
  , CONVERT(CHAR(2), 0x81F4) AS [context-dependent-convert]
  , CAST(0x81F4 AS CHAR(2)) AS [context-dependent-cast]

; WITH uni(c) AS (
    -- BMP character
    SELECT NCHAR(9835)
    UNION ALL
    -- non-BMP supplementary character or, under downlevel collation, NULL
    SELECT NCHAR(127925)
  ),
  enc(u16c, u8c) AS (
    SELECT c, CONVERT(VARCHAR(4), c COLLATE Latin1_General_100_CI_AI_SC_UTF8)
    FROM uni
  )
  SELECT u16c AS [Music note]
    , u8c AS [Music note (UTF-8)]
    , UNICODE(u16c) AS [Code Point]
    , CONVERT(VARBINARY(4), u16c) AS [UTF-16LE bytes]
    , CONVERT(VARBINARY(4), u8c)  AS [UTF-8 bytes]
  FROM enc

DECLARE @document VARCHAR(64);
SELECT @document = 'Reflectors are vital safety' +
                   ' components of your bicycle.';
SELECT CHARINDEX('bicycle', @document);
GO

DECLARE @document VARCHAR(64);

SELECT @document = 'Reflectors are vital safety' +
                   ' components of your bicycle.';
SELECT CHARINDEX('vital', @document, 5);
GO

DECLARE @document VARCHAR(64);

SELECT @document = 'Reflectors are vital safety' +
                   ' components of your bicycle.';
SELECT CHARINDEX('bike', @document);
GO

USE tempdb;
GO
--perform a case sensitive search
SELECT CHARINDEX ( 'TEST',
       'This is a Test'
       COLLATE Latin1_General_CS_AS);

USE tempdb;
GO
SELECT CHARINDEX ( 'Test',
       'This is a Test'
       COLLATE Latin1_General_CS_AS);

USE tempdb;
GO
SELECT CHARINDEX ( 'TEST',
       'This is a Test'
       COLLATE Latin1_General_CI_AS);
GO

SELECT CHARINDEX('is', 'This is a string');

SELECT CHARINDEX('is', 'This is a string', 4);

SELECT TOP(1) CHARINDEX('at', 'This is a string') FROM dbo.DimCustomer;

SELECT CONCAT ( 'Happy ', 'Birthday ', 11, '/', '25' ) AS Result;

CREATE TABLE #temp (
    emp_name NVARCHAR(200) NOT NULL,
    emp_middlename NVARCHAR(200) NULL,
    emp_lastname NVARCHAR(200) NOT NULL
);
INSERT INTO #temp VALUES( 'Name', NULL, 'Lastname' );
SELECT CONCAT( emp_name, emp_middlename, emp_lastname ) AS Result
FROM #temp;

SELECT CONCAT_WS( ' - ', database_id, recovery_model_desc, containment_desc) AS DatabaseInfo
FROM sys.databases;

SELECT CONCAT_WS(',','1 Microsoft Way', NULL, NULL, 'Redmond', 'WA', 98052) AS Address;

SELECT
STRING_AGG(CONCAT_WS( ',', database_id, recovery_model_desc, containment_desc), char(13)) AS DatabaseInfo
FROM sys.databases

SELECT
STRING_AGG(CONCAT_WS( ',', database_id, ISNULL(recovery_model_desc,''), ISNULL(containment_desc,'N/A')), char(13)) AS DatabaseInfo
FROM sys.databases;

-- Returns a DIFFERENCE value of 4, the least possible difference.
SELECT SOUNDEX('Green'), SOUNDEX('Greene'), DIFFERENCE('Green','Greene');
GO
-- Returns a DIFFERENCE value of 0, the highest possible difference.
SELECT SOUNDEX('Blotchet-Halls'), SOUNDEX('Greene'), DIFFERENCE('Blotchet-Halls', 'Greene');
GO

DECLARE @d DATE = '11/22/2020';
SELECT FORMAT( @d, 'd', 'en-US' ) 'US English'
      ,FORMAT( @d, 'd', 'en-gb' ) 'Great Britain English'
      ,FORMAT( @d, 'd', 'de-de' ) 'German'
      ,FORMAT( @d, 'd', 'zh-cn' ) 'Chinese Simplified (PRC)';

SELECT FORMAT( @d, 'D', 'en-US' ) 'US English'
      ,FORMAT( @d, 'D', 'en-gb' ) 'Great Britain English'
      ,FORMAT( @d, 'D', 'de-de' ) 'German'
      ,FORMAT( @d, 'D', 'zh-cn' ) 'Chinese Simplified (PRC)';

DECLARE @d DATE = GETDATE();
SELECT FORMAT( @d, 'dd/MM/yyyy', 'en-US' ) AS 'Date'
       ,FORMAT(123456789,'###-##-####') AS 'Custom Number';

SELECT TOP(5) CurrencyRateID, EndOfDayRate
            ,FORMAT(EndOfDayRate, 'N', 'en-us') AS 'Numeric Format'
            ,FORMAT(EndOfDayRate, 'G', 'en-us') AS 'General Format'
            ,FORMAT(EndOfDayRate, 'C', 'en-us') AS 'Currency Format'
FROM Sales.CurrencyRate
ORDER BY CurrencyRateID;

SELECT TOP(5) CurrencyRateID, EndOfDayRate
      ,FORMAT(EndOfDayRate, 'N', 'de-de') AS 'Numeric Format'
      ,FORMAT(EndOfDayRate, 'G', 'de-de') AS 'General Format'
      ,FORMAT(EndOfDayRate, 'C', 'de-de') AS 'Currency Format'
FROM Sales.CurrencyRate
ORDER BY CurrencyRateID;

SELECT FORMAT(cast('07:35' as time), N'hh.mm');   --> returns NULL
SELECT FORMAT(cast('07:35' as time), N'hh:mm');   --> returns NULL

SELECT FORMAT(cast('07:35' as time), N'hh\.mm');  --> returns 07.35
SELECT FORMAT(cast('07:35' as time), N'hh\:mm');  --> returns 07:35

SELECT FORMAT(SYSDATETIME(), N'hh:mm tt'); -- returns 03:46 PM
SELECT FORMAT(SYSDATETIME(), N'hh:mm t'); -- returns 03:46 P

select FORMAT(CAST('2018-01-01 01:00' AS datetime2), N'hh:mm tt') -- returns 01:00 AM
select FORMAT(CAST('2018-01-01 01:00' AS datetime2), N'hh:mm t')  -- returns 01:00 A

select FORMAT(CAST('2018-01-01 14:00' AS datetime2), N'hh:mm tt') -- returns 02:00 PM
select FORMAT(CAST('2018-01-01 14:00' AS datetime2), N'hh:mm t') -- returns 02:00 P

select FORMAT(CAST('2018-01-01 14:00' AS datetime2), N'HH:mm') -- returns 14:00
SELECT LEFT(Name, 5)
FROM Production.Product
ORDER BY ProductID;
GO

SELECT LEFT('abcdefg',2);
GO

-- Uses AdventureWorks

SELECT LEFT(EnglishProductName, 5)
FROM dbo.DimProduct
ORDER BY ProductKey;

-- Uses AdventureWorks

SELECT LEFT('abcdefg',2) FROM dbo.DimProduct;

DECLARE @v1 VARCHAR(40),
    @v2 NVARCHAR(40);
SELECT
@v1 = 'Test of 22 characters ',
@v2 = 'Test of 22 characters ';
SELECT LEN(@v1) AS [VARCHAR LEN] , DATALENGTH(@v1) AS [VARCHAR DATALENGTH];
SELECT LEN(@v2) AS [NVARCHAR LEN], DATALENGTH(@v2) AS [NVARCHAR DATALENGTH];

SELECT LEN(FirstName) AS Length, FirstName, LastName
FROM Sales.vIndividualCustomer
WHERE CountryRegionName = 'Australia';
GO

USE AdventureWorks2016
GO
SELECT DISTINCT LEN(FirstName) AS FNameLength, FirstName, LastName
FROM dbo.DimEmployee AS e
INNER JOIN dbo.DimGeography AS g
    ON e.SalesTerritoryKey = g.SalesTerritoryKey
WHERE EnglishCountryRegionName = 'Australia';

-- Uses AdventureWorks

SELECT LOWER(SUBSTRING(EnglishProductName, 1, 20)) AS Lower,
       UPPER(SUBSTRING(EnglishProductName, 1, 20)) AS Upper,
       LOWER(UPPER(SUBSTRING(EnglishProductName, 1, 20))) As LowerUpper
FROM dbo.DimProduct
WHERE ListPrice between 11.00 and 20.00;

SELECT LTRIM('     Five spaces are at the beginning of this string.');

DECLARE @string_to_trim VARCHAR(60);
SET @string_to_trim = '     5 spaces are at the beginning of this string.';
SELECT
    @string_to_trim AS 'Original string',
    LTRIM(@string_to_trim) AS 'Without spaces';
GO

CREATE DATABASE test COLLATE Finnish_Swedish_100_CS_AS_SC;
DECLARE @d NVARCHAR(10) = N'𣅿';
-- Old style method.
SELECT NCHAR(0xD84C) + NCHAR(0xDD7F);

-- Preferred method.
SELECT NCHAR(143743);

-- Alternative preferred method.
SELECT NCHAR(UNICODE(@d));

DECLARE @nstring NCHAR(8);
SET @nstring = N'København';
SELECT UNICODE(SUBSTRING(@nstring, 2, 1)),
   NCHAR(UNICODE(SUBSTRING(@nstring, 2, 1)));
GO

-- The @position variable holds the position of the character currently
-- being processed. The @nstring variable is the Unicode character
-- string to process.
DECLARE @position INT, @nstring NCHAR(9);
-- Initialize the current position variable to the first character in
-- the string.
SET @position = 1;
-- Initialize the character string variable to the string to process.
-- Notice that there is an N before the start of the string. This
-- indicates that the data following the N is Unicode data.
SET @nstring = N'København';
-- Print the character number of the position of the string you are at,
-- the actual Unicode character you are processing, and the UNICODE
-- value for this particular character.
PRINT 'Character #' + ' ' + 'Unicode Character' + ' ' + 'UNICODE Value';
WHILE @position <= DATALENGTH(@nstring)
   BEGIN
   SELECT @position,
      NCHAR(UNICODE(SUBSTRING(@nstring, @position, 1))),
      CONVERT(NCHAR(17), SUBSTRING(@nstring, @position, 1)),
      UNICODE(SUBSTRING(@nstring, @position, 1))
   SELECT @position = @position + 1
   END;
GO

SELECT position = PATINDEX('%ter%', 'interesting data');

SELECT position = PATINDEX('%ensure%',DocumentSummary)
FROM Production.Document
WHERE DocumentNode = 0x7B40;
GO

SELECT position = PATINDEX('%en_ure%', 'Please ensure the door is locked!');

SELECT position = PATINDEX('%[^ 0-9A-Za-z]%', 'Please ensure the door is locked!');

USE tempdb;
GO
SELECT PATINDEX ( '%ein%', 'Das ist ein Test'  COLLATE Latin1_General_BIN) ;
GO

DECLARE @MyValue VARCHAR(10) = 'safety';
SELECT position = PATINDEX('%' + @MyValue + '%', DocumentSummary)
FROM Production.Document
WHERE DocumentNode = 0x7B40;

SELECT QUOTENAME('abc[]def');

DECLARE @columnName NVARCHAR(255)='user''s "custom" name'
DECLARE @sql NVARCHAR(MAX) = 'SELECT FirstName AS ' + QUOTENAME(@columnName) + ' FROM dbo.DimCustomer'

EXEC sp_executesql @sql

SELECT QUOTENAME('abc def');

SELECT REPLACE('abcdefghicde','cde','xxx');
GO

SELECT REPLACE('This is a Test'  COLLATE Latin1_General_BIN,
'Test', 'desk' );
GO

DECLARE @STR NVARCHAR(100), @LEN1 INT, @LEN2 INT;
SET @STR = N'This is a sentence with spaces in it.';
SET @LEN1 = LEN(@STR);
SET @STR = REPLACE(@STR, N' ', N'');
SET @LEN2 = LEN(@STR);
SELECT N'Number of spaces in the string: ' + CONVERT(NVARCHAR(20), @LEN1 - @LEN2);

GO

SELECT [Name]
, REPLICATE('0', 4) + [ProductLine] AS 'Line Code'
FROM [Production].[Product]
WHERE [ProductLine] = 'T'
ORDER BY [Name];
GO

IF EXISTS(SELECT name FROM sys.tables
      WHERE name = 't1')
   DROP TABLE t1;
GO
CREATE TABLE t1
(
 c1 varchar(3),
 c2 char(3)
);
GO
INSERT INTO t1 VALUES ('2', '2'), ('37', '37'),('597', '597');
GO
SELECT REPLICATE('0', 3 - DATALENGTH(c1)) + c1 AS 'Varchar Column',
       REPLICATE('0', 3 - DATALENGTH(c2)) + c2 AS 'Char Column'
FROM t1;
GO

-- Uses AdventureWorks

SELECT EnglishProductName AS Name,
   ProductAlternateKey AS ItemCode,
   REPLICATE('0', 4) + ProductAlternateKey AS FullItemCode
FROM dbo.DimProduct
ORDER BY Name;

SELECT FirstName, REVERSE(FirstName) AS Reverse
FROM Person.Person
WHERE BusinessEntityID < 5
ORDER BY FirstName;
GO

DECLARE @myvar VARCHAR(10);
SET @myvar = 'sdrawkcaB';
SELECT REVERSE(@myvar) AS Reversed ;
GO

SELECT REVERSE(1234) AS Reversed ;
GO

SELECT name, REVERSE(name) FROM sys.databases;
GO

SELECT RIGHT(FirstName, 5) AS 'First Name'
FROM Person.Person
WHERE BusinessEntityID < 5
ORDER BY FirstName;
GO

-- Uses AdventureWorks

SELECT RIGHT(LastName, 5) AS Name
FROM dbo.DimEmployee
ORDER BY EmployeeKey;

SELECT RIGHT('abcdefg', 2);

SELECT RTRIM('Removes trailing spaces.   ');

SELECT RTRIM('Four spaces are after the period in this sentence.    ') + 'Next string.';

DECLARE @string_to_trim VARCHAR(60);
SET @string_to_trim = 'Four spaces are after the period in this sentence.    ';
SELECT @string_to_trim + ' Next string.';
SELECT RTRIM(@string_to_trim) + ' Next string.';
GO

-- Using SOUNDEX
SELECT SOUNDEX ('Smith'), SOUNDEX ('Smythe');

-- Using DIFFERENCE
SELECT DIFFERENCE('Smithers', 'Smythers');
GO

SELECT DIFFERENCE('Anothers', 'Brothers');
GO

USE AdventureWorks2012;
GO
SELECT RTRIM(LastName) + ',' + SPACE(2) +  LTRIM(FirstName)
FROM Person.Person
ORDER BY LastName, FirstName;
GO

-- Uses AdventureWorks

SELECT RTRIM(LastName) + ',' + SPACE(2) +  LTRIM(FirstName)
FROM dbo.DimCustomer
ORDER BY LastName, FirstName;
GO

SELECT STR(123.45, 6, 1);
GO

SELECT STR(123.45, 2, 2);
GO

SELECT STR (FLOOR (123.45), 8, 3);
GO

USE AdventureWorks2019
GO
SELECT STRING_AGG (CONVERT(NVARCHAR(max),FirstName), CHAR(13)) AS csv
FROM Person.Person;
GO

USE AdventureWorks2019
GO
SELECT STRING_AGG(CONVERT(NVARCHAR(max), ISNULL(FirstName,'N/A')), ',') AS csv
FROM Person.Person;
GO

USE AdventureWorks2019
GO
SELECT STRING_AGG(CONVERT(NVARCHAR(max), CONCAT(FirstName, ' ', LastName, '(', ModifiedDate, ')')), CHAR(13)) AS names
FROM Person.Person;
GO

SELECT a.articleId, title, STRING_AGG (tag, ',') as tags
FROM dbo.Article AS a
LEFT JOIN dbo.ArticleTag AS t
    ON a.ArticleId = t.ArticleId
GROUP BY a.articleId, title;
GO

USE AdventureWorks2019
GO

SELECT TOP 10 City, STRING_AGG(CONVERT(NVARCHAR(max), EmailAddress), ';') AS emails
FROM Person.BusinessEntityAddress AS BEA
INNER JOIN Person.Address AS A ON BEA.AddressID = A.AddressID
INNER JOIN Person.EmailAddress AS EA ON BEA.BusinessEntityID = EA.BusinessEntityID
GROUP BY City;
GO

USE AdventureWorks2019
GO

SELECT TOP 10 City, STRING_AGG(CONVERT(NVARCHAR(max), EmailAddress), ';') WITHIN GROUP (ORDER BY EmailAddress ASC) AS Emails
FROM Person.BusinessEntityAddress AS BEA
INNER JOIN Person.Address AS A ON BEA.AddressID = A.AddressID
INNER JOIN Person.EmailAddress AS EA ON BEA.BusinessEntityID = EA.BusinessEntityID
GROUP BY City;
GO

SELECT STRING_ESCAPE('\   /
\\    "     ', 'json') AS escapedText;

SET @json = FORMATMESSAGE('{ "id": %d,"name": "%s", "surname": "%s" }',
    17, STRING_ESCAPE(@name,'json'), STRING_ESCAPE(@surname,'json') );

SELECT STUFF('abcdef', 2, 3, 'ijklmn');

DECLARE @VAR INT;
SET @VAR = 3;
SELECT STUFF('abcdef', 2, @VAR, 'ijklmn');
GO

SELECT name, SUBSTRING(name, 1, 1) AS Initial ,
SUBSTRING(name, 3, 2) AS ThirdAndFourthCharacters
FROM sys.databases
WHERE database_id < 5;

SELECT x = SUBSTRING('abcdef', 2, 3);

USE pubs;
SELECT pub_id, SUBSTRING(logo, 1, 10) AS logo,
   SUBSTRING(pr_info, 1, 10) AS pr_info
FROM pub_info
WHERE pub_id = '1756';

IF EXISTS (SELECT table_name FROM INFORMATION_SCHEMA.TABLES
      WHERE table_name = 'npub_info')
   DROP TABLE npub_info;
GO
-- Create npub_info table in pubs database. Borrowed from instpubs.sql.
USE pubs;
GO
CREATE TABLE npub_info
(
 pub_id CHAR(4) NOT NULL
    REFERENCES publishers(pub_id)
    CONSTRAINT UPKCL_npubinfo PRIMARY KEY CLUSTERED,
pr_info ntext NULL
);

GO

-- Fill the pr_info column in npub_info with international data.
RAISERROR('Now at the inserts to pub_info...',0,1);

GO

INSERT npub_info VALUES('0736', N'üThis is sample text data for New Moon Books, publisher 0736 in the pubs database')
,('0877', N'üThis is sample text data for Binnet & Hardley, publisher 0877 in the pubs databa')
,('1389', N'üThis is sample text data for Algodata Infosystems, publisher 1389 in the pubs da')
,('9952', N'üThis is sample text data for Scootney Books, publisher 9952 in the pubs database')
,('1622', N'üThis is sample text data for Five Lakes Publishing, publisher 1622 in the pubs d')
,('1756', N'üThis is sample text data for Ramona Publishers, publisher 1756 in the pubs datab')
,('9901', N'üThis is sample text data for GGG&G, publisher 9901 in the pubs database. GGG&G i')
,('9999', N'üThis is sample text data for Lucerne Publishing, publisher 9999 in the pubs data');
GO
-- Join between npub_info and pub_info on pub_id.
SELECT pr.pub_id, SUBSTRING(pr.pr_info, 1, 35) AS pr_info,
   SUBSTRING(npr.pr_info, 1, 35) AS npr_info
FROM pub_info pr INNER JOIN npub_info npr
   ON pr.pub_id = npr.pub_id
ORDER BY pr.pub_id ASC;

-- Uses AdventureWorks

SELECT LastName, SUBSTRING(FirstName, 1, 1) AS Initial
FROM dbo.DimEmployee
WHERE LastName LIKE 'Bar%'
ORDER BY LastName;

USE ssawPDW;

SELECT TOP 1 SUBSTRING('abcdef', 2, 3) AS x FROM dbo.DimCustomer;

SELECT TRANSLATE('2*[3+4]/{7-2}', '[]{}', '()()');

SELECT
REPLACE
(
      REPLACE
      (
            REPLACE
            (
                  REPLACE
                  (
                        '2*[3+4]/{7-2}',
                        '[',
                        '('
                  ),
                  ']',
                  ')'
            ),
            '{',
            '('
      ),
      '}',
      ')'
);

SELECT TRANSLATE('[137.4,72.3]' , '[,]', '( )') AS Point,
    TRANSLATE('(137.4 72.3)' , '( )', '[,]') AS Coordinates;

SELECT TRANSLATE('abcdef','abc','bcd') AS Translated,
       REPLACE(REPLACE(REPLACE('abcdef','a','b'),'b','c'),'c','d') AS Replaced;

SELECT TRIM( '     test    ') AS Result;

SELECT TRIM( '.,! ' FROM  '     #     test    .') AS Result;

DECLARE @nstring NCHAR(12);
SET @nstring = N'Åkergatan 24';
SELECT UNICODE(@nstring), NCHAR(UNICODE(@nstring));

-- The @position variable holds the position of the character currently
-- being processed. The @nstring variable is the Unicode character
-- string to process.
DECLARE @position INT, @nstring NCHAR(12);
-- Initialize the current position variable to the first character in
-- the string.
SET @position = 1;
-- Initialize the character string variable to the string to process.
-- Notice that there is an N before the start of the string, which
-- indicates that the data following the N is Unicode data.
SET @nstring = N'Åkergatan 24';
-- Print the character number of the position of the string you are at,
-- the actual Unicode character you are processing, and the UNICODE
-- value for this particular character.
PRINT 'Character #' + ' ' + 'Unicode Character' + ' ' + 'UNICODE Value';
WHILE @position <= LEN(@nstring)
-- While these are still characters in the character string,

BEGIN;
   SELECT @position AS [position],
      SUBSTRING(@nstring, @position, 1) AS [character],
      UNICODE(SUBSTRING(@nstring, @position, 1)) AS [code_point];
   SET @position = @position + 1;
END;

-- Uses AdventureWorks

SELECT UPPER(RTRIM(LastName)) + ', ' + FirstName AS Name
FROM dbo.DimEmployee
ORDER BY LastName;
