-- B54A10A.ADA

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
--     CHECK THAT THE BASE TYPE OF THE CASE EXPRESSION AND THE CHOICE
--     MUST NOT BE DIFFERENT.

-- HISTORY:
--     BCB 02/29/88  CREATED ORIGINAL TEST.
--     PWN 11/05/95  REMOVED CHECKS WHERE THE UNIVERSAL INTEGR TYPE
--                   REQUIREMENT HAS BEEN REMOVED.
--     PWN 03/21/96  Restored checks in Ada 95 legal format.

PROCEDURE B54A10A IS

     TYPE X IS NEW INTEGER;

     SUBTYPE INT1 IS INTEGER;

     W : CONSTANT := 100;

     Y : CONSTANT X := 50;

     A : CONSTANT INTEGER := 75;

     C : CONSTANT INT1 := 25;

BEGIN
     CASE Y IS
          WHEN A => NULL;                              -- ERROR:
          WHEN OTHERS => NULL;
     END CASE;

     CASE A IS
          WHEN Y => NULL;                              -- ERROR:
          WHEN OTHERS => NULL;
     END CASE;

     CASE 1_000 IS
          WHEN A => NULL;                              -- OK.
          WHEN OTHERS => NULL;
     END CASE;

     CASE W IS
          WHEN A => NULL;                              -- OK.
          WHEN OTHERS => NULL;
     END CASE;

     CASE C IS
          WHEN A => NULL;                              -- OK.
          WHEN Y => NULL;                              -- ERROR:
          WHEN OTHERS => NULL;
     END CASE;

END B54A10A;
