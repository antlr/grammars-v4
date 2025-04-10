-- Query Plan Name Stated, QCD Name Is TLE_Queries
INSERT EXPLAIN
INTO TLE_Queries AS "Employee Smith Query"
    SELECT id, address
    FROM employee
    WHERE name = 'Ivanov';

-- Workload Execution Frequency Clause Specified
INSERT EXPLAIN WITH STATISTICS FOR table_1
INTO Wizard_QCD AS "Wizard Test" FOR 10
    SELECT *
    FROM table_1
    WHERE table_1.column_2 = 10
    AND table_1.column_3 = 20;

-- Specifying Statistical Sampling
INSERT EXPLAIN WITH STATISTICS
USING SAMPLE 80 PERCENT
INTO MyQCD AS query1
    SELECT t2.y3, t1.x3
    FROM t1, t2
    WHERE t1.pi = t2.y2;

-- Capturing Output Data as an XML Document With No Associated DDL Text
INSERT EXPLAIN INTO myqcd IN XML NODDLTEXT
SELECT *
FROM DBC.DBCInfoV;
