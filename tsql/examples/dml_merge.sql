
--https://docs.microsoft.com/en-us/sql/t-sql/statements/merge-transact-sql

MERGE Production.UnitMeasure AS tgt
USING (SELECT @UnitMeasureCode, @Name) AS src (UnitMeasureCode, Name)
ON (tgt.UnitMeasureCode = src.UnitMeasureCode)
WHEN MATCHED THEN
    UPDATE SET Name = src.Name
WHEN NOT MATCHED THEN
INSERT (UnitMeasureCode, Name)
VALUES (src.UnitMeasureCode, src.Name)
OUTPUT deleted.*, $action, inserted.* INTO #MyTempTable;