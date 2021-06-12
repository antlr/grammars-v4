CREATE INDEX test_index ON TABLE test_table (key) as 'COMPACT' WITH DEFERRED REBUILD;

ALTER INDEX test_index ON test_table partition(t='2021-01-01') REBUILD;

DROP INDEX test_index ON test_table;