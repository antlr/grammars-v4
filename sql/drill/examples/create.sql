create table dfs.tmp.tt as select 1 as c;
create table dfs.tmp.t4 as (with r as (select 1 as c) select * from r);
