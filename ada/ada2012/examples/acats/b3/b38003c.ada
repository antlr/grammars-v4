-- B38003C.ADA

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
--     F) PARAMETER DECLARATION
--     G) RETURN TYPE IN FUNCTION DECLARATION

--        1) WHEN INDEX CONSTRAINT IS PROVIDED IN ACCESS TYPE DEF'N.
--        2) WHEN DISCRIMINANT CONSTRAINT IS PROVIDED IN ACCESS TYPE
--           DEFINITION.
--        3) WHEN SUBTYPE INDICATION IS ALREADY CONSTRAINED.

-- HISTORY:
--      MCH 05/17/90  SPLIT FROM B38003A.ADA.
--      JRL 03/10/93  REMOVED EXTRA "IS" FROM G1, G2, AND G3.

PROCEDURE B38003C IS

     TYPE ARR IS ARRAY(INTEGER RANGE <>) OF INTEGER;
     TYPE ARR_NAME IS ACCESS ARR(1..5);               -- 1.
     SUBTYPE SUB_ARR IS ARR(1..5);                    -- 3.
     TYPE ARR_NAME3 IS ACCESS SUB_ARR;                -- 3.

     TYPE REC(DISC : INTEGER) IS
          RECORD
               NULL;
          END RECORD;

     TYPE REC_NAME IS ACCESS REC(5);                  -- 2.

     PROCEDURE PROC1 (PARM1 : IN OUT ARR_NAME (1..5);      -- ERROR: F1.
                      PARM2 : IN OUT REC_NAME (5);         -- ERROR: F2.
                      PARM3 : IN OUT ARR_NAME3 (1..5)) IS  -- ERROR: F3.
     BEGIN
          PARM1 := NEW ARR(1..5);
          PARM2 := NEW REC(5);
          PARM3 := NEW SUB_ARR;
     END PROC1;

     FUNCTION FUNC1 RETURN ARR_NAME (1..5) IS          -- ERROR: G1.
          F1 : ARR_NAME;
     BEGIN
          F1 := NEW ARR(1..5);
          RETURN F1;
     END FUNC1;

     FUNCTION FUNC2 RETURN REC_NAME (5) IS             -- ERROR: G2.
          F2 : REC_NAME;
     BEGIN
          F2 := NEW REC(5);
          RETURN F2;
     END FUNC2;

     FUNCTION FUNC3 RETURN ARR_NAME3 (1..5) IS         -- ERROR: G3.
          F3 : ARR_NAME3;
     BEGIN
          F3 := NEW SUB_ARR;
          RETURN F3;
     END FUNC3;

 BEGIN
     NULL;
END B38003C;
