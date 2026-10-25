CREATE MATERIALIZED VIEW cycling.cyclist_by_age 
AS SELECT age, name, country 
FROM cycling.cyclist_mv 
WHERE age IS NOT NULL AND cid IS NOT NULL 
PRIMARY KEY (age, cid)
WITH caching = { 'keys' : 'ALL', 'rows_per_partition' : '100' }
   AND comment = 'Based on table cyclist' ;