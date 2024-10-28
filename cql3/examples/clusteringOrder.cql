CREATE TABLE cycling.race_winners (
   a int,
   b int,
   c int,
   PRIMARY KEY ((a), b, c)
)
WITH CLUSTERING ORDER BY (b ASC, c DESC)
AND default_time_to_live = 300;
