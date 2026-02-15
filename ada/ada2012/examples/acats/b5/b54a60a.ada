-- B54A60A.ADA

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
--     CHECK THAT A CASE EXPRESSION MAY NOT BE AN OVERLOADED ENUMERATION
--     LITERAL OR AN OVERLOADED FUNCTION CALL.

-- HISTORY:
--     BCB 02/29/88  CREATED ORIGINAL TEST.

PROCEDURE B54A60A IS

     TYPE INT IS NEW INTEGER;

     TYPE ENUM1 IS (RED, BLUE, GREEN, YELLOW);
     TYPE ENUM2 IS (ORANGE, PURPLE, BLACK, RED, BLUE);

     FUNCTION FUNC RETURN INTEGER IS
     BEGIN
          RETURN 0;
     END FUNC;

     FUNCTION FUNC RETURN INT IS
     BEGIN
          RETURN 0;
     END FUNC;

BEGIN

     CASE RED IS                                       -- ERROR:
          WHEN OTHERS => NULL;
     END CASE;

     CASE FUNC IS                                      -- ERROR:
          WHEN 0 => NULL;
          WHEN OTHERS => NULL;
     END CASE;

END B54A60A;
