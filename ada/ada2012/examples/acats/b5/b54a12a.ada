-- B54A12A.ADA

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
--     CHECK THAT WHEN THE CASE EXPRESSION IS THE NAME OF A CONSTANT
--     OR VARIABLE HAVING A STATIC SUBTYPE, OR IS A QUALIFIED
--     EXPRESSION OR TYPE CONVERSION WITH A STATIC SUBTYPE, NO CHOICE
--     MAY HAVE A VALUE OUTSIDE THE SUBTYPE'S RANGE.

-- HISTORY:
--     BCB 02/29/88  CREATED ORIGINAL TEST.

PROCEDURE B54A12A IS

     SUBTYPE INT IS INTEGER RANGE 1 .. 100;

     A : INT;
     B : CONSTANT INT := 50;
     C : INTEGER;

BEGIN
     CASE A IS
          WHEN 0 => NULL;                -- ERROR: OUTSIDE RANGE OF
                                         --        VARIABLE'S SUBTYPE.
          WHEN 100 => NULL;              -- OK.
          WHEN OTHERS => NULL;           -- OK.
     END CASE;

     CASE A IS
          WHEN 1 => NULL;                -- OK.
          WHEN 101 => NULL;              -- ERROR: OUTSIDE RANGE OF
                                         --        VARIABLE'S SUBTYPE.
          WHEN OTHERS => NULL;           -- OK.
     END CASE;

     CASE B IS
          WHEN 0 => NULL;                -- ERROR: OUTSIDE RANGE OF
                                         --        CONSTANT'S SUBTYPE.
          WHEN 100 => NULL;              -- OK.
          WHEN OTHERS => NULL;           -- OK.
     END CASE;

     CASE B IS
          WHEN 1 => NULL;                -- OK.
          WHEN 101 => NULL;              -- ERROR: OUTSIDE RANGE OF
                                         --        CONSTANT'S SUBTYPE.
          WHEN OTHERS => NULL;           -- OK.
     END CASE;

     CASE INT'(50) IS
          WHEN 0 => NULL;                -- ERROR: OUTSIDE RANGE OF
                                         --        QUALIFIER'S SUBTYPE.
          WHEN 100 => NULL;              -- OK.
          WHEN OTHERS => NULL;           -- OK.
     END CASE;

     CASE INT'(50) IS
          WHEN 1 => NULL;                -- OK.
          WHEN 101 => NULL;              -- ERROR: OUTSIDE RANGE OF
                                         --        QUALIFIER'S SUBTYPE.
          WHEN OTHERS => NULL;           -- OK.
     END CASE;

     CASE INT(C) IS
          WHEN 0 => NULL;                -- ERROR: OUTSIDE RANGE OF
                                         --        CONVERSION'S SUBTYPE.
          WHEN 100 => NULL;              -- OK.
          WHEN OTHERS => NULL;           -- OK.
     END CASE;

     CASE INT(C) IS
          WHEN 1 => NULL;                -- OK.
          WHEN 101 => NULL;              -- ERROR: OUTSIDE RANGE OF
                                         --        CONVERSION'S SUBTYPE.
          WHEN OTHERS => NULL;           -- OK.
     END CASE;

END B54A12A;
