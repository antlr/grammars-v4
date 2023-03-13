select * from dfs.tmp.tt;
select true;
select null;
select 1=1;
with r as (select 1 as c) select * from r;
select * from (with r as (select 1 as c) select * from r ) as f;
select t.d from t1 as t(d);
select t1.* from t1 cross join t2 ;
