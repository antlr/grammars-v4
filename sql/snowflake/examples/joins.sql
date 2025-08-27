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
natural full outer directed join u using(a);

select *
from t
cross directed join u;
