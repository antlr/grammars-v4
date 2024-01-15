with t as (
    select current_time() as ct, current_date() as cd, current_timestamp() as cts
)
select *
from t
where
    ct > TIME '0:0:0' or
    cd > DATE '2024-01-01' or
    cts > TIMESTAMP '2024-01-01 0:0:0';
