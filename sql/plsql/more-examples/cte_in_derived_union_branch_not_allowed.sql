select * from dual
union all
(with cte as (select 1 from dual) select * from cte);