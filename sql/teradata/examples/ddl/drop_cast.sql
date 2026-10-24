-- Dropping the Cast of a Geospatial UDT to a Geospatial UDT
DROP CAST (circle AS ellipse);

-- DROP CAST (circle AS ellipse);
DROP CAST (circle AS VARCHAR(32));

-- Dropping the System-Generated Cast from Source Data Type to Distinct UDT
DROP CAST (DECIMAL AS euro);
