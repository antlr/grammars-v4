-- B54A01L.ADA

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
-- CHECK THAT 'OTHERS' IN CASE STATEMENTS MUST BE LAST
-- AND THAT IT MUST BE THE ONLY CHOICE.

-- DAT 3/18/81
-- ABW 6/11/82

PROCEDURE B54A01L IS

     I : INTEGER := 4;

BEGIN

     CASE I IS
          WHEN 5 => NULL;
          WHEN OTHERS => NULL;          -- ERROR: OTHERS NOT LAST.
          WHEN 6 => NULL;
     END CASE;

     CASE I IS
          WHEN OTHERS => NULL;          -- ERROR: OTHERS NOT LAST.
          WHEN 7 => NULL;
     END CASE;

     CASE I IS
          WHEN 7 | OTHERS => NULL;      -- ERROR: OTHERS NOT ALONE.
     END CASE;

     CASE I IS
          WHEN 7 => NULL;
          WHEN OTHERS | 5 => NULL;      -- ERROR: OTHERS NOT ALONE.
     END CASE;

     CASE I IS
          WHEN 7 => NULL;
          WHEN 1 | OTHERS | 3 => NULL;  -- ERROR: OTHERS NOT ALONE.
     END CASE;

     CASE I IS
          WHEN 3 | 4 => NULL;
          WHEN 1 | OTHERS => NULL;      -- ERROR: OTHERS NOT ALONE.
     END CASE;

END B54A01L;
