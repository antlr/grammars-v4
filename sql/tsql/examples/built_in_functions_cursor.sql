USE AdventureWorks2012;
GO
SELECT @@CURSOR_ROWS;
DECLARE Name_Cursor CURSOR FOR
SELECT LastName ,@@CURSOR_ROWS FROM Person.Person;
OPEN Name_Cursor;
FETCH NEXT FROM Name_Cursor;
SELECT @@CURSOR_ROWS;
CLOSE Name_Cursor;
DEALLOCATE Name_Cursor;
GO

DECLARE Employee_Cursor CURSOR FOR
SELECT BusinessEntityID, JobTitle
FROM AdventureWorks2012.HumanResources.Employee;
OPEN Employee_Cursor;
FETCH NEXT FROM Employee_Cursor;
WHILE @@FETCH_STATUS = 0
   BEGIN
      FETCH NEXT FROM Employee_Cursor;
   END;
CLOSE Employee_Cursor;
DEALLOCATE Employee_Cursor;
GO

CREATE TABLE #TMP
(
   ii INT
)
GO

INSERT INTO #TMP(ii) VALUES(1)
INSERT INTO #TMP(ii) VALUES(2)
INSERT INTO #TMP(ii) VALUES(3)

GO

--Create a cursor.
DECLARE cur CURSOR
FOR SELECT * FROM #TMP

--Display the status of the cursor before and after opening
--closing the cursor.

SELECT CURSOR_STATUS('global','cur') AS 'After declare'
OPEN cur
SELECT CURSOR_STATUS('global','cur') AS 'After Open'
CLOSE cur
SELECT CURSOR_STATUS('global','cur') AS 'After Close'

--Remove the cursor.
DEALLOCATE cur

--Drop the table.
DROP TABLE #TMP
