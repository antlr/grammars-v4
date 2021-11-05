--
-- \crosstabview
--

CREATE TABLE ctv_data (v, h, c, i, d) AS
VALUES
   ('v1','h2','foo', 3, '2015-04-01'::date),
   ('v2','h1','bar', 3, '2015-01-02'),
   ('v1','h0','baz', NULL, '2015-07-12'),
   ('v0','h4','qux', 4, '2015-07-15'),
   ('v0','h4','dbl', -3, '2014-12-15'),
   ('v0',NULL,'qux', 5, '2014-07-15'),
   ('v1','h2','quux',7, '2015-04-04');

-- make plans more stable
ANALYZE ctv_data;

-- running \crosstabview after query uses query in buffer
SELECT v, EXTRACT(year FROM d), count(*)
 FROM ctv_data
 GROUP BY 1, 2
 ORDER BY 1, 2;
-- basic usage with 3 columns
 \crosstabview

-- ordered months in horizontal header, quoted column name
SELECT v, to_char(d, 'Mon') AS "month name", EXTRACT(month FROM d) AS num,
 count(*) FROM ctv_data  GROUP BY 1,2,3 ORDER BY 1
 \crosstabview v "month name" 4 num

-- ordered months in vertical header, ordered years in horizontal header
SELECT EXTRACT(year FROM d) AS year, to_char(d,'Mon') AS """month"" name",
  EXTRACT(month FROM d) AS month,
  format('sum=%s avg=%s', sum(i), avg(i)::numeric(2,1))
  FROM ctv_data
  GROUP BY EXTRACT(year FROM d), to_char(d,'Mon'), EXTRACT(month FROM d)
ORDER BY month
\crosstabview """month"" name" year format year

-- combine contents vertically into the same cell (V/H duplicates)
SELECT v, h, string_agg(c, E'\n') FROM ctv_data GROUP BY v, h ORDER BY 1,2,3
 \crosstabview 1 2 3

-- horizontal ASC order from window function
SELECT v,h, string_agg(c, E'\n') AS c, row_number() OVER(ORDER BY h) AS r
FROM ctv_data GROUP BY v, h ORDER BY 1,3,2
 \crosstabview v h c r

-- horizontal DESC order from window function
SELECT v, h, string_agg(c, E'\n') AS c, row_number() OVER(ORDER BY h DESC) AS r
FROM ctv_data GROUP BY v, h ORDER BY 1,3,2
 \crosstabview v h c r

-- horizontal ASC order from window function, NULLs pushed rightmost
SELECT v,h, string_agg(c, E'\n') AS c, row_number() OVER(ORDER BY h NULLS LAST) AS r
FROM ctv_data GROUP BY v, h ORDER BY 1,3,2
 \crosstabview v h c r

-- only null, no column name, 2 columns: error
SELECT null,null \crosstabview

-- only null, no column name, 3 columns: works
SELECT null,null,null \crosstabview

-- null display
\pset null '#null#'
SELECT v,h, string_agg(i::text, E'\n') AS i FROM ctv_data
GROUP BY v, h ORDER BY h,v
 \crosstabview v h i
\pset null ''

-- refer to columns by position
SELECT v,h,string_agg(i::text, E'\n'), string_agg(c, E'\n')
FROM ctv_data GROUP BY v, h ORDER BY h,v
 \crosstabview 2 1 4

-- refer to columns by positions and names mixed
SELECT v,h, string_agg(i::text, E'\n') AS i, string_agg(c, E'\n') AS c
FROM ctv_data GROUP BY v, h ORDER BY h,v
 \crosstabview 1 "h" 4

-- refer to columns by quoted names, check downcasing of unquoted name
SELECT 1 as "22", 2 as b, 3 as "Foo"
 \crosstabview "22" B "Foo"

-- error: bad column name
SELECT v,h,c,i FROM ctv_data
 \crosstabview v h j

-- error: need to quote name
SELECT 1 as "22", 2 as b, 3 as "Foo"
 \crosstabview 1 2 Foo

-- error: need to not quote name
SELECT 1 as "22", 2 as b, 3 as "Foo"
 \crosstabview 1 "B" "Foo"

-- error: bad column number
SELECT v,h,i,c FROM ctv_data
 \crosstabview 2 1 5

-- error: same H and V columns
SELECT v,h,i,c FROM ctv_data
 \crosstabview 2 h 4

-- error: too many columns
SELECT a,a,1 FROM generate_series(1,3000) AS a
 \crosstabview

-- error: only one column
SELECT 1 \crosstabview

DROP TABLE ctv_data;

-- check error reporting (bug #14476)
CREATE TABLE ctv_data (x int, y int, v text);

INSERT INTO ctv_data SELECT 1, x, '*' || x FROM generate_series(1,10) x;
SELECT * FROM ctv_data \crosstabview

INSERT INTO ctv_data VALUES (1, 10, '*'); -- duplicate data to cause error
SELECT * FROM ctv_data \crosstabview

DROP TABLE ctv_data;
