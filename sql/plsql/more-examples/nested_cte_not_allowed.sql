with cte_outer as (
   with cte_inner as (select 1 from dual)
   select * from cte_inner
)
select * from cte_outer;