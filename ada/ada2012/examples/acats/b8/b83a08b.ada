-- B83A08B.ADA

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
--     A STATEMENT LABEL DECLARED OUTSIDE A BLOCK AND HAVING THE SAME
--     IDENTIFIER AS AN ENTITY DECLARED IN THE BLOCK IS ILLEGAL AS THE
--     TARGET OF A GOTO STATEMENT INSIDE THE BLOCK.

-- HISTORY:
--     PMW 09/07/88  CREATED ORIGINAL TEST.

PROCEDURE B83A08B IS

     PASSES : INTEGER := 0;

BEGIN

     <<LBL>>

     IF PASSES > 0 THEN
          GOTO ENOUGH;
     END IF;

     BEGIN
          DECLARE
               LBL : INTEGER := 1;
          BEGIN
               PASSES := PASSES + 1;

               GOTO LBL;            -- ERROR: LABEL OUTSIDE BLOCK
          END;
     END;

     PASSES := 0;

     <<LBL2>>

     IF PASSES > 0 THEN
          GOTO ENOUGH;
     END IF;

     BEGIN
          DECLARE
              FUNCTION LBL2 RETURN BOOLEAN IS
              BEGIN
                   RETURN TRUE;
              END;
          BEGIN
               PASSES := PASSES + 1;

               GOTO LBL2;          -- ERROR: LABEL #2 OUTSIDE BLOCK
          END;
     END;
     PASSES := 0;

     <<ENOUGH>>

     NULL;

END B83A08B;
