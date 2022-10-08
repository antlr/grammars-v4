CREATE MATERIALIZED VIEW a AS SELECT * FROM t;
CREATE OR REPLACE MATERIALIZED VIEW catalog.schema.matview COMMENT 'A simple materialized view' AS SELECT * FROM catalog2.schema2.tab;
CREATE OR REPLACE MATERIALIZED VIEW catalog.schema.matview COMMENT 'A simple materialized view' AS SELECT * FROM catalog2.schema2.tab;
CREATE OR REPLACE MATERIALIZED VIEW catalog.schema.matview COMMENT 'A simple materialized view'WITH (partitioned_by = ARRAY ['dateint']) AS SELECT * FROM catalog2.schema2.tab;
CREATE OR REPLACE MATERIALIZED VIEW catalog.schema.matview COMMENT 'A partitioned materialized view' WITH (partitioned_by = ARRAY ['dateint']) AS WITH a (t, u) AS (SELECT * FROM x), b AS (SELECT * FROM a) TABLE b;
