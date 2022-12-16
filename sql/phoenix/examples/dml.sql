/* SELECT */
select c,d from t;
select c,d from s.t as c where c=1 order by c;
select true and false and not false from t;
select c from t where c = 1 and d = 1 or e = 1;
select array[1,2] as a from t;
select f(a, b) from t;
select case w when 1 then 2 when 3 then 4 else 5 end as c from t;
select case when w=1 then 2 when w=3 then 4 else 5 end as c from t;
select c from t where c between 1+1 and 2*3 and d is not null and e in (1,2);
select c from t where not a and b;
select * from t where exists (select * from tt);
select * from t where not exists (select * from tt);
select c.* from t as c;
select * from (select * from t) as t;
select * from t inner join tt on t.i = tt.d;
select * from t left join tt on t.i = tt.d;
select * from t left join tt on t.i = tt.d limit 1;

/* UPSERT VALUES */
upsert into t (c,d) values(1,2);
upsert into t (c,d) values(1,2) on duplicate key ignore;
upsert into t (c,d) values(1,2) on duplicate key update c = 1;

/* UPSERT SELECT */
upsert into t(c, d) select a, b from t;
upsert into t(c, d) select a, b from t where c = 1;

/* DELETE */
delete from t where c = 1 and d = 2;
delete from t where c = 1 order by c, d;
delete from t where c = 1 order by c limit 1;
