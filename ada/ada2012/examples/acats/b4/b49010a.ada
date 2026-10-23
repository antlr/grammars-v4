-- B49010A.ADA

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
-- CHECK THAT A STATIC EXPRESSION CANNOT CONTAIN A QUALIFIED EXPRESSION
-- IF THE TYPE MARK DENOTES A NONSTATIC TYPE (SCALAR OR NOT), OR THE
-- ARGUMENT IS A NONSTATIC SCALAR EXPRESSION.

-- RJW 2/26/86

PROCEDURE B49010A IS

BEGIN
     DECLARE
          TYPE DAY IS (SUN, MON, TUE, WED, THUR, FRI, SAT);
          M : DAY := MON;
          F : DAY := FRI;
          SUBTYPE WEEKDAY IS DAY RANGE M .. F;
          TYPE PTR IS ACCESS DAY;
          P : CONSTANT PTR := NEW DAY'(MON);
          Q : CONSTANT PTR := NEW DAY'(TUE);
          B : BOOLEAN;

     BEGIN
          CASE B IS
               WHEN (DAY'(SUN) = DAY'(MON)) =>   -- OK.
                    NULL;
               WHEN (PTR'(P) = PTR'(Q)) =>       -- ERROR: NONSTATIC
                                                 --        NON SCALAR
                                                 --        TYPE MARK.
                    NULL;
               WHEN (WEEKDAY'(WED) = WEEKDAY'(FRI)) =>  -- ERROR:
                                                        -- NONSTATIC
                                                        -- SCALAR TYPE
                                                        -- MARK.
                    NULL;
               WHEN (DAY'(M) = DAY'(F)) =>       -- ERROR: NONSTATIC
                                                 --        ARGUMENT.
                    NULL;
               WHEN OTHERS =>
                    NULL;
          END CASE;
     END;

END B49010A;
