select *
from t
inner directed join u on a=b;

select *
from t
left outer directed join u on a=b;

select *
from t
left directed join u on a=b;

select *
from t
full outer directed join u using(i);

select *
from t
natural full outer directed join u;

select *
from t
cross directed join u;

select *
from t1
natural join t2;

select *
from t1
natural inner directed join t2;

select *
from t1
full join t2 on i=j;

select *
from t1
full join t2 using(i);

select *
from t
cross join u
cross join v;

select *
from t
asof join u match_condition (ts1 = ts2 and c1 = c2);

select *
from t
asof join u match_condition (ts1 = ts2 and c1 = c2) on (i=j);

select *
from t
asof join u match_condition (ts1 = ts2 and c1 = c2) using (i, j);