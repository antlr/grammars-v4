
--https://docs.microsoft.com/en-us/sql/t-sql/statements/merge-transact-sql

MERGE Production.UnitMeasure AS target
USING (SELECT @UnitMeasureCode, @Name) AS source (UnitMeasureCode, Name)
ON (target.UnitMeasureCode = source.UnitMeasureCode)
WHEN MATCHED THEN
    UPDATE SET Name = source.Name
WHEN NOT MATCHED THEN
INSERT (UnitMeasureCode, Name)
VALUES (source.UnitMeasureCode, source.Name)
OUTPUT deleted.*, $action, inserted.* INTO #MyTempTable;