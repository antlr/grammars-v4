select * from tb;

with a as (select * from tb) select a,b,c from a;