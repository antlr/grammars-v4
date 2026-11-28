SELECT branches, channels, middle_name, regions, warehouse_size
FROM (SELECT 1 AS branches, 2 AS channels, 'Q' AS middle_name, 3 AS regions, 'XSMALL' AS warehouse_size) AS t;
WITH regions AS (SELECT 1 AS id, 'EU' AS name), channels AS (SELECT 10 AS id, 1 AS region_id)
SELECT regions.name, COUNT(channels.id) AS channels FROM regions JOIN channels ON channels.region_id = regions.id GROUP BY regions.name;
CREATE TEMPORARY TABLE branches (branches INT, channels INT, middle_name VARCHAR, regions INT, warehouse_size VARCHAR);
INSERT INTO branches (branches, channels, middle_name, regions, warehouse_size) VALUES (1, 2, 'Q', 3, 'XSMALL');
SELECT branches.middle_name, branches.warehouse_size FROM branches WHERE branches.regions = 3;
