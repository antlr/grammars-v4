ALTER MATERIALIZED VIEW cycling.cyclist_by_age 
WITH comment = 'A most excellent and useful view'
AND bloom_filter_fp_chance = 0.02;
