alter materialized view mv1 suspend recluster;
alter materialized view mv1 suspend;
alter materialized view mv1 resume recluster;
alter materialized view mv1 resume;
CREATE MATERIALIZED VIEW V1 (C1 COMMENT 'TEST') AS SELECT 1 as C1;
CREATE MATERIALIZED VIEW V1 (C1) AS SELECT 1 as C1;
