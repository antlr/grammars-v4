create table t(i int);

alter table t add column if not exists j int;
alter table t drop column if exists i;
ALTER TABLE t ADD ROW ACCESS POLICY RLS ON (j);
ALTER TABLE t DROP ROW ACCESS POLICY RLS;
ALTER TABLE t DROP ROW ACCESS POLICY RLS , ADD ROW ACCESS POLICY RLS ON (j);
