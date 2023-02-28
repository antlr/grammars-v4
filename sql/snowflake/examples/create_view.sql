CREATE VIEW RAW.ViewName AS Select MyField FROM RAW.TABLE1;
create view v as select 1 as c;
CREATE VIEW RAW.ViewName AS WITH src AS (Select MyField FROM RAW.TABLE1 WHERE MyField IS NOT NULL) SELECT MyField FROM SRC;