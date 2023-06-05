--
-- PG_LSN
--

CREATE TABLE PG_LSN_TBL (f1 pg_lsn);

-- Largest and smallest input
INSERT INTO PG_LSN_TBL VALUES ('0/0');
INSERT INTO PG_LSN_TBL VALUES ('FFFFFFFF/FFFFFFFF');

-- Incorrect input
INSERT INTO PG_LSN_TBL VALUES ('G/0');
INSERT INTO PG_LSN_TBL VALUES ('-1/0');
INSERT INTO PG_LSN_TBL VALUES (' 0/12345678');
INSERT INTO PG_LSN_TBL VALUES ('ABCD/');
INSERT INTO PG_LSN_TBL VALUES ('/ABCD');

-- Min/Max aggregation
SELECT MIN(f1), MAX(f1) FROM PG_LSN_TBL;

DROP TABLE PG_LSN_TBL;

-- Operators
SELECT '0/16AE7F8' = '0/16AE7F8'::pg_lsn;
SELECT '0/16AE7F8'::pg_lsn != '0/16AE7F7';
SELECT '0/16AE7F7' < '0/16AE7F8'::pg_lsn;
SELECT '0/16AE7F8' > pg_lsn '0/16AE7F7';
SELECT '0/16AE7F7'::pg_lsn - '0/16AE7F8'::pg_lsn;
SELECT '0/16AE7F8'::pg_lsn - '0/16AE7F7'::pg_lsn;
SELECT '0/16AE7F7'::pg_lsn + 16::numeric;
SELECT 16::numeric + '0/16AE7F7'::pg_lsn;
SELECT '0/16AE7F7'::pg_lsn - 16::numeric;
SELECT 'FFFFFFFF/FFFFFFFE'::pg_lsn + 1::numeric;
SELECT 'FFFFFFFF/FFFFFFFE'::pg_lsn + 2::numeric; -- out of range error
SELECT '0/1'::pg_lsn - 1::numeric;
SELECT '0/1'::pg_lsn - 2::numeric; -- out of range error
SELECT '0/0'::pg_lsn + ('FFFFFFFF/FFFFFFFF'::pg_lsn - '0/0'::pg_lsn);
SELECT 'FFFFFFFF/FFFFFFFF'::pg_lsn - ('FFFFFFFF/FFFFFFFF'::pg_lsn - '0/0'::pg_lsn);
SELECT '0/16AE7F7'::pg_lsn + 'NaN'::numeric;
SELECT '0/16AE7F7'::pg_lsn - 'NaN'::numeric;

-- Check btree and hash opclasses
EXPLAIN (COSTS OFF)
SELECT DISTINCT (i || '/' || j)::pg_lsn f
  FROM generate_series(1, 10) i,
       generate_series(1, 10) j,
       generate_series(1, 5) k
  WHERE i <= 10 AND j > 0 AND j <= 10
  ORDER BY f;

SELECT DISTINCT (i || '/' || j)::pg_lsn f
  FROM generate_series(1, 10) i,
       generate_series(1, 10) j,
       generate_series(1, 5) k
  WHERE i <= 10 AND j > 0 AND j <= 10
  ORDER BY f;
