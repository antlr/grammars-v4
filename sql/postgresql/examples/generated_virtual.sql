-- Virtual generated columns (PostgreSQL 18): GENERATED ALWAYS AS (expr) [VIRTUAL | STORED].
-- Statements taken from src/test/regress/sql/generated_virtual.sql.

CREATE TABLE gtest0 (a int PRIMARY KEY, b int GENERATED ALWAYS AS (55) VIRTUAL);

CREATE TABLE gtest1 (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest_err_2a (a int PRIMARY KEY, b int GENERATED ALWAYS AS (b * 2) VIRTUAL);

CREATE TABLE gtest_err_2b (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL, c int GENERATED ALWAYS AS (b * 3) VIRTUAL);

CREATE TABLE gtest_err_2c (a int PRIMARY KEY,
    b int GENERATED ALWAYS AS (num_nulls(gtest_err_2c)) VIRTUAL);

CREATE TABLE gtest_err_3 (a int PRIMARY KEY, b int GENERATED ALWAYS AS (c * 2) VIRTUAL);

CREATE TABLE gtest_err_4 (a int PRIMARY KEY, b double precision GENERATED ALWAYS AS (random()) VIRTUAL);

CREATE TABLE gtest2 (a int, b text GENERATED ALWAYS AS (a || ' sec') VIRTUAL);

CREATE TABLE gtest_err_6a (a int PRIMARY KEY, b bool GENERATED ALWAYS AS (xmin <> 37) VIRTUAL);

CREATE TABLE gtest_err_7a (a int PRIMARY KEY, b int GENERATED ALWAYS AS (avg(a)) VIRTUAL);

CREATE TABLE gtest_err_7b (a int PRIMARY KEY, b int GENERATED ALWAYS AS (row_number() OVER (ORDER BY a)) VIRTUAL);

CREATE TABLE gtest_err_7c (a int PRIMARY KEY, b int GENERATED ALWAYS AS ((SELECT a)) VIRTUAL);

CREATE TABLE gtest_err_7d (a int PRIMARY KEY, b int GENERATED ALWAYS AS (generate_series(1, a)) VIRTUAL);

CREATE TABLE gtestm (
  id int PRIMARY KEY,
  f1 int,
  f2 int,
  f3 int GENERATED ALWAYS AS (f1 * 2) VIRTUAL,
  f4 int GENERATED ALWAYS AS (f2 * 2) VIRTUAL
);

CREATE TABLE gtest_normal_child (a int, b int GENERATED ALWAYS AS (a * 2) VIRTUAL) INHERITS (gtest_normal);

CREATE TABLE gtestx (x int, b int GENERATED ALWAYS AS (a * 22) VIRTUAL) INHERITS (gtest1);

CREATE TABLE gtestxx_3 (a int NOT NULL, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtestxx_4 (b int GENERATED ALWAYS AS (a * 2) VIRTUAL, a int NOT NULL);

CREATE TABLE gtesty (x int, b int GENERATED ALWAYS AS (x * 22) VIRTUAL);

CREATE TABLE gtest1_y (b int GENERATED ALWAYS AS (x + 1) VIRTUAL) INHERITS (gtest1, gtesty);

CREATE TABLE gtestc (f2 int GENERATED ALWAYS AS (f1+1) VIRTUAL) INHERITS(gtestp);

CREATE TABLE gtest3 (a int, b int GENERATED ALWAYS AS (a * 3) VIRTUAL);

CREATE TABLE gtest3a (a text, b text GENERATED ALWAYS AS (a || '+' || a) VIRTUAL);

CREATE TABLE gtest2 (a int PRIMARY KEY, b int GENERATED ALWAYS AS (NULL) VIRTUAL);

CREATE TABLE gtest_varlena (a varchar, b varchar GENERATED ALWAYS AS (a) VIRTUAL);

CREATE TABLE gtest4 (
    a int,
    b double_int GENERATED ALWAYS AS ((a * 2, a * 3)) VIRTUAL
);

CREATE TABLE gtest_tableoid (
  a int PRIMARY KEY,
  b bool GENERATED ALWAYS AS (tableoid = 'gtest_tableoid'::regclass) VIRTUAL
);

ALTER TABLE gtest_tableoid ADD COLUMN
  c regclass GENERATED ALWAYS AS (tableoid) VIRTUAL;

CREATE TABLE gtest10 (a int PRIMARY KEY, b int, c int GENERATED ALWAYS AS (b * 2) VIRTUAL);

CREATE TABLE gtest10a (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest11 (a int PRIMARY KEY, b int, c int GENERATED ALWAYS AS (b * 2) VIRTUAL);

CREATE TABLE gtest12 (a int PRIMARY KEY, b int, c int GENERATED ALWAYS AS (gf1(b)) VIRTUAL);

CREATE TABLE gtest20 (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL CHECK (b < 50));

CREATE TABLE gtest20a (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest20b (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest20c (a int, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest21a (a int PRIMARY KEY, b int GENERATED ALWAYS AS (nullif(a, 0)) VIRTUAL NOT NULL);

CREATE TABLE gtest21ax (a int PRIMARY KEY, b int GENERATED ALWAYS AS (nullif(a, 0)) VIRTUAL);

CREATE TABLE gtest21b (a int, b int GENERATED ALWAYS AS (nullif(a, 0)) VIRTUAL);

CREATE TABLE gtest21c (a int NOT NULL, b int GENERATED ALWAYS AS (a * 2) VIRTUAL NOT NULL, c int NOT NULL);

ALTER TABLE gtest21d ADD COLUMN b INT GENERATED ALWAYS AS (a * 10) VIRTUAL NOT NULL;

CREATE TABLE gtestnn_parent (
    f1 int,
    f2 bigint,
    f3 bigint GENERATED ALWAYS AS (nullif(f1, 1) + nullif(f2, 10)) VIRTUAL NOT NULL
) PARTITION BY RANGE (f1);

ALTER TABLE gtestnn_parent ADD COLUMN c int NOT NULL GENERATED ALWAYS AS (nullif(f1, 14) + nullif(f2, 10)) VIRTUAL;

CREATE TABLE gtest22a (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a / 2) VIRTUAL UNIQUE);

CREATE TABLE gtest22b (a int, b int GENERATED ALWAYS AS (a / 2) VIRTUAL, PRIMARY KEY (a, b));

CREATE TABLE gtest22c (a int, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest23x (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL REFERENCES gtest23a (x) ON UPDATE CASCADE);

CREATE TABLE gtest23x (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL REFERENCES gtest23a (x) ON DELETE SET NULL);

CREATE TABLE gtest23b (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL REFERENCES gtest23a (x));

CREATE TABLE gtest23p (x int, y int GENERATED ALWAYS AS (x * 2) VIRTUAL, PRIMARY KEY (y));

CREATE TABLE gtest24 (a int PRIMARY KEY, b gtestdomain1 GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest24r (a int PRIMARY KEY, b gtestdomain1range GENERATED ALWAYS AS (gtestdomain1range(a, a + 5)) VIRTUAL);

ALTER TABLE gtest24at ADD COLUMN b gtestdomain1 GENERATED ALWAYS AS (a * 2) VIRTUAL;

CREATE TABLE gtest24ata (a int PRIMARY KEY, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest24nn (a int, b gtestdomainnn GENERATED ALWAYS AS (a * 2) VIRTUAL);

CREATE TABLE gtest24xxx (a gtestdomain1, b gtestdomain1, c int GENERATED ALWAYS AS (greatest(a, b)) VIRTUAL);

CREATE TABLE gtest28 OF gtest_type (f1 WITH OPTIONS GENERATED ALWAYS AS (f2 *2) VIRTUAL);

CREATE TABLE gtest_child PARTITION OF gtest_parent (
    f3 WITH OPTIONS GENERATED ALWAYS AS (f2 * 2) VIRTUAL
) FOR VALUES FROM ('2016-07-01') TO ('2016-08-01');

CREATE TABLE gtest_child (f1 date NOT NULL, f2 bigint, f3 bigint GENERATED ALWAYS AS (f2 * 2) VIRTUAL);

CREATE TABLE gtest_parent (f1 date NOT NULL, f2 bigint, f3 bigint GENERATED ALWAYS AS (f2 * 2) VIRTUAL) PARTITION BY RANGE (f1);

CREATE TABLE gtest_child2 PARTITION OF gtest_parent (
    f3 WITH OPTIONS GENERATED ALWAYS AS (f2 * 22) VIRTUAL  -- overrides gen expr
) FOR VALUES FROM ('2016-08-01') TO ('2016-09-01');

CREATE TABLE gtest_child3 (f1 date NOT NULL, f2 bigint, f3 bigint GENERATED ALWAYS AS (f2 * 33) VIRTUAL);

CREATE TABLE gtest_part_key (f1 date NOT NULL, f2 bigint, f3 bigint GENERATED ALWAYS AS (f2 * 2) VIRTUAL) PARTITION BY RANGE (f3);

CREATE TABLE gtest_part_key (f1 date NOT NULL, f2 bigint, f3 bigint GENERATED ALWAYS AS (f2 * 2) VIRTUAL) PARTITION BY RANGE ((f3));

ALTER TABLE gtest25 ADD COLUMN x int GENERATED ALWAYS AS (b * 4) VIRTUAL;

ALTER TABLE gtest25 ADD COLUMN c int DEFAULT 42,
  ADD COLUMN x int GENERATED ALWAYS AS (c * 4) VIRTUAL;

ALTER TABLE gtest25 ALTER COLUMN d SET DATA TYPE float8,
  ADD COLUMN y float8 GENERATED ALWAYS AS (d * 4) VIRTUAL;

CREATE TABLE gtest27 (
    a int,
    b int,
    x int GENERATED ALWAYS AS ((a + b) * 2) VIRTUAL
);

ALTER TABLE gtest27
  DROP COLUMN x,
  ALTER COLUMN a TYPE bigint,
  ALTER COLUMN b TYPE bigint,
  ADD COLUMN x bigint GENERATED ALWAYS AS ((a + b) * 2) VIRTUAL NOT NULL;

CREATE TABLE gtest29 (
    a int,
    b int GENERATED ALWAYS AS (a * 2) VIRTUAL
);

CREATE TABLE gtest30 (
    a int,
    b int GENERATED ALWAYS AS (a * 2) VIRTUAL
);

CREATE TABLE gtest31_1 (a int, b text GENERATED ALWAYS AS ('hello') VIRTUAL, c text);

CREATE TABLE gtest31_1 (a int, b text GENERATED ALWAYS AS ('hello') VIRTUAL, c text) PARTITION BY LIST (a);

CREATE TABLE gtest26 (
    a int PRIMARY KEY,
    b int GENERATED ALWAYS AS (a * 2) VIRTUAL
);

CREATE TABLE gtest28a (
  a int,
  b int,
  c int,
  x int GENERATED ALWAYS AS (b * 2) VIRTUAL
);

CREATE TABLE gtest_rule (a int, b int GENERATED ALWAYS AS (a * 2) VIRTUAL);

create table gtest32 (
  a int primary key,
  b int generated always as (a * 2),
  c int generated always as (10 + 10),
  d int generated always as (coalesce(a, 100)),
  e int
);

create table gtest33 (a int, b int generated always as (a * 2) virtual not null, check (b > 10));

create table gtest34 (id int primary key, a int,
                      c int generated always as (a * 10) virtual);

create table gtest34p (id int primary key, a int,
                       c int generated always as (a * 10) virtual)
    partition by range (id);

create table gtest_cursor (id int primary key, a int, b int generated always as (a * 2) virtual);
