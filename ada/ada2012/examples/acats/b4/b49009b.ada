-- B49009B.ADA

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
-- CHECK THAT A STATIC EXPRESSION CANNOT CONTAIN THE ATTRIBUTES 'POS,
-- 'VAL, 'SUCC, OR 'PRED IF THE PREFIX OF THESE ATTRIBUTES DENOTES A
-- NONSTATIC SUBTYPE OR THE ARGUMENT IS A NONSTATIC EXPRESSION.

-- L.BROWN  08/29/86

PROCEDURE  B49009B  IS

     TYPE ENUM IS (RED,YELLOW,BLUE,GREEN,BLACK,WHITE);
     OBJ1 : ENUM := GREEN;
     SUBTYPE SUENUM IS ENUM RANGE RED .. OBJ1;
     TYPE INT IS RANGE 1 .. SUENUM'POS(YELLOW);                -- ERROR:
     OBJ2 : BOOLEAN := FALSE;
     X : INTEGER := 2;
BEGIN
     CASE OBJ1 IS
          WHEN SUENUM'PRED(YELLOW) =>                          -- ERROR:
               OBJ2 := TRUE;
          WHEN OTHERS =>
               NULL;
     END CASE;

     CASE OBJ1 IS
          WHEN SUENUM'SUCC(YELLOW) =>                          -- ERROR:
               OBJ2 := TRUE;
          WHEN OTHERS =>
               NULL;
     END CASE;

     CASE OBJ1 IS
          WHEN SUENUM'VAL(3) =>                                -- ERROR:
               OBJ2 := TRUE;
          WHEN OTHERS =>
               NULL;
     END CASE;

     CASE  X IS
          WHEN ENUM'POS(OBJ1) =>                               -- ERROR:
               OBJ2 := TRUE;
          WHEN OTHERS =>
               NULL;
     END CASE;

     CASE OBJ1 IS
          WHEN ENUM'VAL(X) =>                                  -- ERROR:
               OBJ2 := TRUE;
          WHEN OTHERS =>
               NULL;
     END CASE;

END B49009B;
