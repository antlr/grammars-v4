create table t1(i int) engine = MergeTree() order by i;
insert into t1 values(1); insert into t1 values(2);