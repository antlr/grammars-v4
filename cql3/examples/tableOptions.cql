CREATE TABLE alphabet (
   a int,
   b int,
   c int,
   PRIMARY KEY ((a), b, c)
)
WITH COMPACT STORAGE
AND CLUSTERING ORDER BY (b ASC, c DESC)
AND default_time_to_live = 300;
