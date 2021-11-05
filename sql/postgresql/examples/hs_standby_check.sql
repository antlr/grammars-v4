--
-- Hot Standby tests
--
-- hs_standby_check.sql
--

--
-- If the query below returns false then all other tests will fail after it.
--
select case pg_is_in_recovery() when false then
	'These tests are intended only for execution on a standby server that is reading ' ||
	'WAL from a server upon which the regression database is already created and into ' ||
	'which src/test/regress/sql/hs_primary_setup.sql has been run'
else
	'Tests are running on a standby server during recovery'
end;
