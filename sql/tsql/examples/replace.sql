SELECT REPLACE FROM (SELECT 1 AS REPLACE) T
GO

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
