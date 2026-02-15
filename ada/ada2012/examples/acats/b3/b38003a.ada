-- B38003A.ADA

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
--      CHECK THAT IF AN INDEX OR DISCRIMINANT CONSTRAINT IS PROVIDED IN
--      A SUBTYPE DEFINITION OR DIRECTLY IN AN ACCESS TYPE DEFINITION,
--      THE ACCESS TYPE NAME CANNOT SUBSEQUENTLY BE USED WITH AN INDEX
--      OR DISCRIMINANT CONSTRAINT, EVEN IF THE SAME CONSTRAINT VALUES
--      ARE USED.

-- CASES:
--     A) OBJECT DECLARATION
--     B) ARRAY COMPONENT DECLARATION
--     C) RECORD COMPONENT DECLARATION
--     D) ACCESS TYPE DECLARATION
--     E) DERIVED TYPE DEFINITION

--        1) WHEN INDEX CONSTRAINT IS PROVIDED IN ACCESS TYPE DEF'NN.
--        2) WHEN DISCRIMINANT CONSTRAINT IS PROVIDED IN ACCESS TYPE
--           DEFINITION.
--        3) WHEN SUBTYPE INDICATION IS ALREADY CONSTRAINED.

-- HISTORY:
--      ASL 06/22/81  CREATED ORIGINAL TEST.
--      SPS 12/10/82
--      AH  08/25/86  ADDED CASE WITH CONSTRAINED SUBTYPE.
--      MCH 05/17/90  SPLIT AND CREATED B38003C.ADA.

PROCEDURE B38003A IS

     TYPE ARR IS ARRAY(INTEGER RANGE <>) OF INTEGER;
     TYPE ARR_NAME IS ACCESS ARR(1..5);               -- 1.
     SUBTYPE SUB_ARR IS ARR(1..5);                    -- 3.
     TYPE ARR_NAME3 IS ACCESS SUB_ARR;                -- 3.

     TYPE REC(DISC : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE REC_NAME IS ACCESS REC(5);                  -- 2.

     OBJ1 : ARR_NAME(1..5);                        -- ERROR: A1.
     OBJ2 : REC_NAME(5);                           -- ERROR: A2.
     OBJ3 : ARR_NAME3(1..5);                       -- ERROR: A3.


     TYPE ARR2 IS ARRAY(1..10) OF ARR_NAME(1..5);  -- ERROR: B1.
     TYPE ARR3 IS ARRAY(1..10) OF REC_NAME(5);     -- ERROR: B2.
     TYPE ARR4 IS ARRAY(1..10) OF ARR_NAME3(1..5); -- ERROR: B3.

     TYPE REC2 IS
          RECORD
               COMP1 : ARR_NAME(1..5);             -- ERROR: C1.
               COMP2 : REC_NAME(5);                -- ERROR: C2.
               COMP3 : ARR_NAME3(1..5);            -- ERROR: C3.
          END RECORD;

     TYPE AA_NAME IS ACCESS ARR_NAME(1..5);        -- ERROR: D1.
     TYPE AR_NAME IS ACCESS REC_NAME(5);           -- ERROR: D2.
     TYPE AS_NAME IS ACCESS ARR_NAME3(1..5);       -- ERROR: D3.

     TYPE DER1 IS NEW ARR_NAME(1..5);              -- ERROR: E1.
     TYPE DER2 IS NEW REC_NAME(5);                 -- ERROR: E2.
     TYPE DER3 IS NEW ARR_NAME3(1..5);             -- ERROR: E3.

BEGIN
     NULL;
END B38003A;
