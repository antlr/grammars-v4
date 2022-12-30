select
  userhash
  , case when (a = 1) then 1 else 0 end as v1
  , iff(a = 1, 1, 0) as v2
from t
;

-- Table with types and functions as column names
select user, date, iff, current_timestamp, cluster, type, region, function from edge;

-- Time travel https://docs.snowflake.com/en/sql-reference/constructs/at-before.html
-- Examples from doc
select * from my_table at(timestamp => 'Fri, 01 May 2015 16:20:00 -0700'::timestamp);
select * from my_table at(timestamp => to_timestamp(1432669154242, 3));
select * from my_table at(offset => -60*5) as t where t.flag = 'valid';
select * from my_table before(statement => '8e5d0ca9-005e-44e6-b858-a8f5b37c5726');
select oldt.* ,newt.*
  from my_table before(statement => '8e5d0ca9-005e-44e6-b858-a8f5b37c5726') as oldt
    full outer join my_table at(statement => '8e5d0ca9-005e-44e6-b858-a8f5b37c5726') as newt
    on oldt.id = newt.id
where oldt.id is null or newt.id is null;

-- having
select department_id from employees group by department_id having count(*) < 10;