-- Collect Statistics on a Single-Column NUSI
COLLECT STATISTICS FOR SAMPLE 10 PERCENT
INTO MyQCD
ON orders INDEX orderDateNUPI;

-- Collect Statistics On Index Using an Alternate Syntax
COLLECT STATISTICS FOR SAMPLE 10 PERCENT
INTO MyQCD
ON orders INDEX (order_date) ;

-- Collecting Single-Column PARTITION Statistics
COLLECT STATISTICS FOR SAMPLE 30 PERCENT
INTO myqcd UPDATE MODIFIED
ON orders COLUMN PARTITION;

-- Collecting Multi-Column PARTITION Statistics
COLLECT STATISTICS FOR SAMPLE 20 PERCENT
INTO myqcd
ON orders COLUMN (quant_ord, PARTITION, quant_shpd);
