-- Table with types and functions as column names
-- Causes timeout on PHP build test?
-- select user, date, iff, current_timestamp, cluster, type, region, function from edge;
SELECT MAX(src.COL3) OVER (PARTITION BY COL1) as MAXCol3 FROM Table1 AS src;
SELECT MAX(src.COL3) OVER (PARTITION BY COL1 ORDER BY COL2 DESC, COL1 ASC) as MAXCol3 FROM Table1 AS src;
SELECT  MAX(src.COL3) OVER (PARTITION BY COL1 ORDER BY COL2) as MAXCol3 FROM Table1 AS src;
SELECT MAX(src.COL3) OVER (ORDER BY COL2) as MAXCol3 FROM Table1 AS src;
SELECT  MAX(src.COL3) OVER (ORDER BY COL2 DESC, COL1 ASC) as MAXCol3 FROM Table1 AS src;

