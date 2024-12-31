-- Partition Analysis on Specified Tables
INITIATE PARTITION ANALYSIS
ON tab1, tab2
FOR myworkload IN myqcd AS my_analysis;

-- Partition Analysis with a Time Limit
INITIATE PARTITION ANALYSIS
FOR myworkload IN myqcd AS myfastanalysis
TIME LIMIT = 5;
