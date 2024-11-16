-- Input and Output Arguments in BTEQ and CLIv2
CALL spSample2(p1, 20 * 2, 30 + 40);

-- Input and Output Arguments in BTEQ and CLIv2 using NAMED and TITLE phrases in the CALL request
CALL spSample1(CAST (p1 AS NAMED AA TITLE 'OUT VALUE'),
    CAST (((20 * 2) + 3) AS TITLE 'INOUT VALUE'), 1);

-- Stored Procedures and Embedded SQL Input and Output Arguments
CALL spSample1(:Var1, :Var2, :Var3 + 2);

-- Using RETURNS and RETURNS STYLE Clauses in the Same Procedure Call
CALL spSample3(10
           , RESULT_B RETURNS INTEGER
           , RESULT_C
           , RESULT_D RETURNS STYLE t1.int_col
           , RESULT_E
           , RESULT_F RETURNS INTEGER);
