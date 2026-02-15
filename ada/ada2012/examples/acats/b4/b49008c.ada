-- B49008C.ADA

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
-- CHECK THAT IN A STATIC EXPRESSION THE ARGUMENTS OF A FUNCTION CALL
-- CANNOT BE NONSTATIC EXPRESSIONS IF THE FUNCTION NAME IS AN OPERATOR
-- SYMBOL THAT DENOTES A PREDEFINED OPERATOR.

-- L.BROWN  08/27/86

PROCEDURE  B49008C  IS

     TYPE INT IS RANGE 1 .. 15;
     TYPE REAL IS DIGITS 5 RANGE 0.0 .. 25.0;
     OBJ_INT : INT := 5;
     OBJ_REAL : REAL := 3.0;
     CAS_OBJ1 : INT := 6;
     TYPE INT1 IS RANGE 1 .. "+"(1,OBJ_INT);                   -- ERROR:
     TYPE FIX IS DELTA 3.0*("+"(1.0,OBJ_REAL))                 -- ERROR:
                                    RANGE 0.0 .. 24.0;
     TYPE FIX1 IS DELTA 3.0 RANGE 0.0 .. "-"(5.0,OBJ_REAL);    -- ERROR:
BEGIN

     CASE CAS_OBJ1 IS
          WHEN "+"(1,OBJ_INT) =>                               -- ERROR:
               OBJ_INT := 6;
          WHEN OTHERS =>
               NULL;
     END CASE;

END B49008C;
