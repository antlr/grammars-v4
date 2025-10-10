with r1 as (
    select * from t
)
, r2 as (
    values (1, 2) as tt(a, b)
)
select *
from r2;