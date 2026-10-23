-- BC3501B.ADA

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
-- CHECK THAT IF A FORMAL GENERIC TYPE FT IS AN ACCESS TYPE, THE
-- CORRESPONDING ACTUAL TYPE PARAMETER MUST BE AN ACCESS TYPE.
-- CHECK THAT FT DOES NOT MATCH AN ACTUAL PARAMETER THAT IS THE BASE
-- TYPE OF FT'S DESIGNATED TYPE.  CHECK WHEN THE DESIGNATED TYPE IS A
-- GENERIC FORMAL TYPE DECLARED IN THE SAME FORMAL PART AS FT.

-- SPS 5/18/82

PROCEDURE BC3501B IS

     GENERIC
          TYPE T IS (<>);
          TYPE FT IS ACCESS T;
     PACKAGE P IS END P;

     TYPE AI IS ACCESS INTEGER;
     PACKAGE P1 IS NEW P (INTEGER, AI);      -- OK.
     PACKAGE P2 IS NEW P (INTEGER, INTEGER); -- ERROR: INTEGER IS NOT AN
                                             -- ACCESS TYPE.

     TYPE AC IS ACCESS CHARACTER;
     PACKAGE P3 IS NEW P (CHARACTER, AC);    -- OK.
     PACKAGE P4 IS NEW P (CHARACTER, CHARACTER); -- ERROR: CHARACTER IS
                                             -- NOT AN ACCESS TYPE.

     TYPE AB IS ACCESS BOOLEAN;
     PACKAGE P5 IS NEW P (BOOLEAN, AB);      -- OK.
     PACKAGE P6 IS NEW P (BOOLEAN, BOOLEAN); -- ERROR: BOOLEAN IS NOT AN
                                             -- ACCESS TYPE.

     TYPE ENUM IS (E1, E2, E3, E4);
     TYPE AE IS ACCESS ENUM;
     PACKAGE P7 IS NEW P (ENUM, AE);         -- OK.
     PACKAGE P8 IS NEW P (ENUM, ENUM);       -- ERROR: ENUM IS NOT AN
                                             -- ACCESS TYPE.

BEGIN
     NULL;
END BC3501B;
