select userhash, case when (a = 1) then 1 else 0 end as v1, iff(a = 1, 1, 0) as v2 from t;
