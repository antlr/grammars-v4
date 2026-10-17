select
    case when 1 = 1 then (
        with cte as (select 1 cte_col from dual)
        select cte_col from cte
    ) end as res
from dual;

select *
from (
    with cte1 as (
        select 1 id from dual
    )
    select *
    from (
      with cte2 as (
          select 'success!' txt from dual
      )
      select * from cte2
      cross join cte1
   ) c2
) main;
