grant select on table t to r1;
grant select on table t to public;
call p1 (1);
set role r1;
revoke select on table t from r1;
revoke usage on type t1 from r2 restrict;
lock table t in share mode;
