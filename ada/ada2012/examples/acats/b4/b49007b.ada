-- B49007B.ADA

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
--     CHECK THAT A CONSTANT DECLARED BY AN OBJECT DECLARATION CANNOT
--     BE USED IN A STATIC EXPRESSION IF THE SUBTYPE USED IN THE
--     DECLARATION WAS NONSTATIC, OR IF THE CONSTANT WAS INITIALIZED
--     WITH A NONSTATIC EXPRESSION.

-- HISTORY:
--     JET 08/16/88  CREATED ORIGINAL TEST.
--     PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

PROCEDURE B49007B IS

     TYPE ARR1 IS ARRAY (0..10) OF BOOLEAN;

     I : INTEGER := 0;

     C1 : CONSTANT INTEGER := ARR1'FIRST;
     C2 : CONSTANT INTEGER := I;

     TYPE INT2 IS RANGE C2 .. 100;           -- ERROR: NONSTATIC INIT.

     TYPE REC1(D : INTEGER) IS RECORD
          CASE D IS
               WHEN C2 =>                    -- ERROR: NONSTATIC INIT.
                    J : INTEGER;
               WHEN OTHERS =>
                    K : INTEGER;
          END CASE;
     END RECORD;

     TYPE REC2(D : INTEGER := 0) IS RECORD
          CASE D IS
               WHEN 1 => I : INTEGER;
               WHEN OTHERS => J : INTEGER;
          END CASE;
     END RECORD;

     R2 : REC2 := (D => C2, J => 1);         -- ERROR: NONSTATIC INIT.

BEGIN
     CASE INTEGER(1) IS
          WHEN C2 => NULL;                   -- ERROR: NONSTATIC INIT.
          WHEN OTHERS => NULL;
     END CASE;
END B49007B;
