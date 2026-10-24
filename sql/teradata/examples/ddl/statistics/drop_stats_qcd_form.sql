-- Drop Statistics on All Columns
DROP STATISTICS FROM MyQCD
ON orders;

-- Drop Statistics on a Single Column
DROP STATISTICS FROM MyQCD
ON orders INDEX orderDateNUPI;

-- Dropping PARTITION Statistics
DROP STATISTICS FROM QCD_12 ON table_2
COLUMN (column_1, PARTITION);
