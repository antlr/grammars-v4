select
  userhash
  , case when (a = 1) then 1 else 0 end as v1
  , iff(a = 1, 1, 0) as v2
from t
;

-- Table with types and functions as column names
select user, date, iff, current_timestamp, cluster, type, region, function from edge;


-- having
select department_id from employees group by department_id having count(*) < 10;
