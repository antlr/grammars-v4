-- AUTO and ENGINE_ATTRIBUTE are non-reserved keywords: they can be used as identifiers.
CREATE TABLE t_kw (auto INT, engine_attribute JSON);
SELECT auto, engine_attribute FROM t_kw AS auto WHERE auto.auto = 1;
-- Their keyword uses still parse.
ANALYZE TABLE t_kw UPDATE HISTOGRAM ON auto WITH 10 BUCKETS AUTO UPDATE;
CREATE TABLE t_ea (c INT ENGINE_ATTRIBUTE = '{}') ENGINE_ATTRIBUTE = '{}' SECONDARY_ENGINE_ATTRIBUTE = '{}';
