-- BC1009A.ADA

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained 
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making 
--     this public release, the Government intends to confer upon all 
--     recipients unlimited rights  equal to those held by the Government.  
--     These rights include rights to use, duplicate, release or disclose the 
--     released technical data and computer software in whole or in part, in 
--     any manner and for any purpose whatsoever, and to have or permit others 
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED 
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE 
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
-- CHECK THAT GENERIC FORMAL PARAMETER IDENTIFIERS MUST BE DISTINCT
-- FROM EACH OTHER (UNLESS OVERLOADED),  AND MUST NOT BE REDECLARED
-- IN THE SUBPROGRAM OR PACKAGE SPECIFICATION OR THE DECLARATIVE PART
-- OF THE BODY.

-- DAT 8/6/81
-- SPS 10/18/82
-- JBG 10/5/83

PROCEDURE BC1009A IS

     GENERIC -- GENERIC PART
          P1,P1 : INTEGER;                   -- ERROR: TWO P1'S.
          TYPE T1 IS PRIVATE;
          TYPE T1 (D:INTEGER) IS PRIVATE;    -- ERROR: TWO T1'S.
          TYPE T1 (X:BOOLEAN) IS PRIVATE;    -- ERROR: THREE T1'S.
     PACKAGE G1 IS END G1;

     GENERIC -- GENERIC PART
          P3 : INTEGER;
          P2, P3 : INTEGER;             -- ERROR: TWO P3'S.
     PACKAGE G2 IS
     END G2;

     GENERIC -- GENERIC PART AND FORMAL PART
          P4 : INTEGER;
     PROCEDURE G3 (P4 : INTEGER);            -- ERROR: TWO P4'S.

     GENERIC -- GENERIC PART AND SUBPROGRAM BODY
          TYPE T4 (D:INTEGER) IS PRIVATE;
          P5 : INTEGER;                      
     FUNCTION G4 RETURN INTEGER;             

     GENERIC -- GENERIC PART AND PACKAGE SPECIFICATION
          P99, P6, P98 : INTEGER;
          TYPE T5 IS LIMITED PRIVATE;
     PACKAGE G5 IS
          P5, P6, P4 : INTEGER;              -- ERROR: TWO P6'S.
          TYPE T5 IS NEW INTEGER;            -- ERROR: TWO T5'S.
     END G5;

     GENERIC -- GENERIC PART AND PACKAGE BODY
          P7 : INTEGER;
          TYPE T6 IS (<>);
     PACKAGE G6 IS
     END G6;

     FUNCTION G4 RETURN INTEGER IS
          TYPE T4 IS NEW FLOAT;              -- ERROR: TWO T4'S.
          P5 : FLOAT;                        -- ERROR: TWO P5'S.
     BEGIN RETURN 0; END G4;

     PROCEDURE G3 (P4 : INTEGER) IS BEGIN NULL; END;

     PACKAGE BODY G6 IS
          P7 : FLOAT;                        -- ERROR: TWO P7'S.
          TYPE T6 IS NEW INTEGER;            -- ERROR: TWO T6'S.
     BEGIN
          NULL;
     END G6;

BEGIN
     NULL;
END BC1009A;
