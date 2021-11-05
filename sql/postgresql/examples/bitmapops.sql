-- Test bitmap AND and OR


-- Generate enough data that we can test the lossy bitmaps.

-- There's 55 tuples per page in the table. 53 is just
-- below 55, so that an index scan with qual a = constant
-- will return at least one hit per page. 59 is just above
-- 55, so that an index scan with qual b = constant will return
-- hits on most but not all pages. 53 and 59 are prime, so that
-- there's a maximum number of a,b combinations in the table.
-- That allows us to test all the different combinations of
-- lossy and non-lossy pages with the minimum amount of data

CREATE TABLE bmscantest (a int, b int, t text);

INSERT INTO bmscantest
  SELECT (r%53), (r%59), 'foooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo'
  FROM generate_series(1,70000) r;

CREATE INDEX i_bmtest_a ON bmscantest(a);
CREATE INDEX i_bmtest_b ON bmscantest(b);

-- We want to use bitmapscans. With default settings, the planner currently
-- chooses a bitmap scan for the queries below anyway, but let's make sure.
set enable_indexscan=false;
set enable_seqscan=false;

-- Lower work_mem to trigger use of lossy bitmaps
set work_mem = 64;


-- Test bitmap-and.
SELECT count(*) FROM bmscantest WHERE a = 1 AND b = 1;

-- Test bitmap-or.
SELECT count(*) FROM bmscantest WHERE a = 1 OR b = 1;


-- clean up
DROP TABLE bmscantest;
