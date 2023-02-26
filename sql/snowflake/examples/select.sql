-- Table with types and functions as column names
-- Causes timeout on PHP build test?
-- select user, date, iff, current_timestamp, cluster, type, region, function from edge;
SELECT MAX(src.COL3) OVER (PARTITION BY COL1) as MAXCol3 FROM Table1 AS src;
SELECT MAX(src.COL3) OVER (PARTITION BY COL1 ORDER BY COL2 DESC, COL1 ASC) as MAXCol3 FROM Table1 AS src;
SELECT  MAX(src.COL3) OVER (PARTITION BY COL1 ORDER BY COL2) as MAXCol3 FROM Table1 AS src;
SELECT MAX(src.COL3) OVER (ORDER BY COL2) as MAXCol3 FROM Table1 AS src;
SELECT  MAX(src.COL3) OVER (ORDER BY COL2 DESC, COL1 ASC) as MAXCol3 FROM Table1 AS src;
SELECT concat('A',' ','bcd') as result;
SELECT concat('A',' ','bcd');
SELECT 'teststring';  
SELECT 'teststring' AS result;
SELECT  cast(concat(to_date(ColDate, 'yyyyMMdd'), ' ', substring(ColHour, 1, 2), ':', substr(ColHour, 3, 2), ':', substring(ColHour, 5, 2)) as datetime) as FinalDate;
with src as (SELECT 1 as COL1 UNION SELECT 2) SELECT COL1 FROM SRC;