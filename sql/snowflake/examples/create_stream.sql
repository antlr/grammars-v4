create stream s1 on table t1;
create or replace stream str_test2 on table test_str at (STREAM => 'STR_TEST2');
create stream if not exists s1 with tag (mytag='myval') on view vw;