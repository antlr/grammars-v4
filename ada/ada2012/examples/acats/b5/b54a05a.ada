-- B54A05A.ADA

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
-- CHECK THAT NON-DISCRETE (STRING) AND PRIVATE TYPES ARE NOT PERMITTED
--    IN CASE EXPRESSIONS.

-- DAT 1/29/81

PROCEDURE B54A05A IS

     S1 : CONSTANT STRING (1 .. 1) := "A";

     PACKAGE P IS
          TYPE T IS PRIVATE;
          TYPE LT IS LIMITED PRIVATE;
          VT : CONSTANT T;
          VLT : CONSTANT LT;
     PRIVATE
          TYPE T IS ('Z', X);
          TYPE LT IS NEW INTEGER RANGE 0 .. 1;
          VT : CONSTANT T := X;
          VLT : CONSTANT LT := 0;
     END P;
     USE P;

BEGIN
     CASE INTEGER'(3) IS      -- OK.
          WHEN OTHERS => NULL;
     END CASE;

     CASE S1 IS               -- ERROR: STRING TYPE.
          WHEN OTHERS => NULL;
     END CASE;

     CASE VT IS               -- ERROR: PRIVATE TYPE.
          WHEN OTHERS => NULL;
     END CASE;

     CASE VLT IS              -- ERROR: PRIVATE TYPE.
          WHEN OTHERS => NULL;
     END CASE;

END B54A05A;
