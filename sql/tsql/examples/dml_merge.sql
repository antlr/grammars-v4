
--https://docs.microsoft.com/en-us/sql/t-sql/statements/merge-transact-sql

MERGE Production.UnitMeasure AS target
USING (SELECT @UnitMeasureCode, @Name) AS src (UnitMeasureCode, Name)
ON (target.UnitMeasureCode = src.UnitMeasureCode)
WHEN MATCHED THEN
    UPDATE SET Name = src.Name
WHEN NOT MATCHED THEN
INSERT (UnitMeasureCode, Name)
VALUES (src.UnitMeasureCode, src.Name)
OUTPUT deleted.*,
       $action,
       inserted.*,
       CASE WHEN $action = 'INSERT' THEN 1 ELSE 0 END,
       IIF($action = 'DELETE', 1, 0),
       inserted.UnitMeasureCode
INTO #MyTempTable;