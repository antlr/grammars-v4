-- Index Analysis on table list
INITIATE INDEX ANALYSIS ON table_1, table_2
FOR MyWorkload
IN MyQCD
AS table_1Index;

-- Index Analysis and Repeated Parameters
INITIATE INDEX ANALYSIS ON table_1
FOR MyWorkload
IN MyQCD
AS table_1Index
SET IndexesPerTable=2, ChangeRate=4, IndexesPerTable=4;

-- Using a CHECKPOINT
INITIATE INDEX ANALYSIS ON table_1
FOR MyWorkload
IN MyQCD
AS table_1Index
SET IndexesPerTable = 2
KEEP INDEX
CHECKPOINT 10;

-- Specifying a CHECKPOINT and a TIME LIMIT
INITIATE INDEX ANALYSIS
FOR wl1
IN myqcd
AS wl1_analysis1
CHECKPOINT 1
TIME LIMIT = 1;

-- Include NUSI and Simple Join Indexes Only in the Analysis
INITIATE INDEX ANALYSIS ON tab1
FOR MyWorkload
IN MyQCD AS tab1Index
WITH INDEX TYPE 4, 5;
