
WITH abcd as(
    SELECT
        *
    FROM OPENQUERY(mysqldg,'
        select
      * from abcd
        ') AS b
             LEFT JOIN test.dbo.abcde AS p ON
        b.inco=p.gco
),
     plucount as(
         select
             j.loc,
             COUNT(j.plu) as plucount
         from jhdata as j
         group by
             j.loc
         having
             COUNT(j.plu)>1
     )
select
    *
from jhdata as a
         inner join plucount as p on
    a.loc=p.loc
order by
    a.dept asc,
    a.loc asc