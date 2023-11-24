create table t(i int);

alter table t add column if not exists j int;
alter table t drop column if exists i;
