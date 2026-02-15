-- B33205A.ADA

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
-- OBJECTIVE:
--     CHECK THAT A DISCRIMINANT CONSTRAINT IS ILLEGAL IN A SUBTYPE
--     INDICATION IF THE TYPE MARK DENOTES:
--          A SCALAR
--          AN ARRAY
--          A CONSTRAINED RECORD
--          A CONSTRAINED ACCESS
--          A TASK
--          A CONSTRAINED PRIVATE TYPE
--          AN ACCESS TYPE DENOTING ANY OF THE ABOVE.

-- HISTORY:
--     DHH 01/21/88 CREATED ORIGINAL TEST.
--     THS 03/28/90 MOVED "-- ERROR:" TO THE RIGHT OF THE CONSTRUCT
--                  IN ERROR.
--     WMC 04/07/92 REMOVED UNNECESSARY REFERENCES TO PACKAGE REPORT.


PROCEDURE B33205A IS

-- SCALAR
     SUBTYPE INT IS INTEGER RANGE 0 .. 10;
     TYPE INT_ACC IS ACCESS INT;
     SUBTYPE SHORT_INT IS INT(5);           -- ERROR: INTEGER.
     A : INT_ACC(5);               -- ERROR: ACCESS DESIGNATING INTEGER.

     TYPE COLOR IS (RED, BLUE, YELLOW);
     TYPE COLOR_ACC IS ACCESS COLOR;
     SUBTYPE SUB_COLOR IS COLOR(RED);       -- ERROR: ENUMERATION.
     B : COLOR_ACC(RED);          -- ERROR: ACCESS DESIGNATING ENUMER.

     TYPE FLOAT IS DIGITS 5;
     TYPE FLOAT_ACC IS ACCESS FLOAT;
     SUBTYPE SUB_FLOAT IS FLOAT(5.0);       -- ERROR: FLOATING POINT.
     BB : FLOAT_ACC(4.0);           -- ERROR: ACCESS DESIGNATING FLOAT.

-- ARRAY
     TYPE SHORT_ARRAY IS ARRAY(INTEGER RANGE <>) OF INTEGER;
     TYPE ARRAY_PTR IS ACCESS SHORT_ARRAY;
     SUBTYPE FAIL_ARRAY IS SHORT_ARRAY(5);    -- ERROR: ARRAY.
     C : ARRAY_PTR(5);              -- ERROR: ACCESS DESIGNATING ARRAY.

-- CONSTRAINED RECORD
     TYPE MATRIX IS ARRAY(INTEGER RANGE <>) OF INTEGER;
     TYPE SQUARE (X : INTEGER) IS
          RECORD
               MAT : MATRIX(1 .. X);
          END RECORD;
     SUBTYPE NEW_SQUARE IS SQUARE(5);
     TYPE NEW_SQUARE_PTR IS ACCESS NEW_SQUARE;
     SUBTYPE FAIL_SQUARE IS NEW_SQUARE(5);  -- ERROR: CONSTRAINED REC.
     SUBTYPE FAIL_PTR IS NEW_SQUARE_PTR(5); -- ERROR: ACCESS DESIGNATING
                                            -- CONSTRAINED RECORD.

-- TASK
     TASK TYPE T IS
          ENTRY READ(S : INTEGER);
     END T;
     TYPE T_PTR IS ACCESS T;
     SUBTYPE TT IS T(5);                    -- ERROR: TASK TYPE.
     SUBTYPE TT_PTR IS T_PTR(5);     -- ERROR: ACCESS DESIGNATING TASK.

-- CONSTRAINED PRIVATE TYPE
     PACKAGE PACK IS
          TYPE P(X : INTEGER) IS PRIVATE;
          TYPE P_PTR IS ACCESS P;
          SUBTYPE PP IS P(5);
          TYPE AC_PTR IS ACCESS PP(5);      -- ERROR: ACCESS DESIGNATING
                                            --      CONSTRAINED PRIVATE.
          SUBTYPE PP_PTR IS P_PTR(5);
          SUBTYPE PPP IS PP(5);             -- ERROR: CONSTRAINED
                                            --        PRIVATE TYPE.
          SUBTYPE PPP_PTR IS PP_PTR(5);     -- ERROR: ACCESS DESIGNATING
                                            --      CONSTRAINED ACCESS
                                            --       SUBTYPE.

     PRIVATE
          TYPE P(X : INTEGER) IS
               RECORD
                    MAT : MATRIX(1 .. X);
               END RECORD;
     END PACK;
     USE PACK;

-- CONSTRAINED ACCESS
     TYPE CON_ACC IS ACCESS SQUARE(5);
     SUBTYPE SUB_ACC IS CON_ACC(2);        -- ERROR: CONSTRAINED ACCESS.
     TYPE CON_ACC_PTR IS ACCESS CON_ACC;
     AC : CON_ACC_PTR(5);                  -- ERROR: ACCESS DESIGNATING
                                           -- CONSTRAINED ACCESS.
     TASK BODY T IS
          Q : INTEGER;
     BEGIN
          ACCEPT READ(S : INTEGER) DO
               Q := S;
          END READ;
     END T;

BEGIN
     NULL;
END B33205A;
