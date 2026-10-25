select *
from t as a
where c = 1 or d = 1
group by c1, c2
order by c
limit 10;