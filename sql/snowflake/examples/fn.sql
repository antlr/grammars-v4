create table tj as select 1 as c, {'n' : 1} as j;

SELECT *
FROM tj, TABLE(FLATTEN(j)) b;
--
with ni as (
    select 1 as c, {'n' : 1} as j
)
, ni2 AS (
    SELECT c, b.*
    FROM ni, TABLE(FLATTEN(j)) b
)
select * from ni2;
