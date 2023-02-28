with r as (
    select 1 as c, 2 * (3 + 4) as d
    from t
    where t.a between 1 and 2
        and exists ( select 1 as c )
        and b = some (select 1 as c )
        and exists (select 1 as c)
        and a between 1 and 2
        and c=1
        and f(1) = 1
        and c = some (select 1)
        and c like 's'
        and t.c = 1
        and f(g(1,2),3) = 1
        and c is not null
and d is null
)
select *
from r
where c = 1 or d = 2
