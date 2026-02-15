-- BC3501E.ADA

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
-- CHECK THAT FT DOES NOT MATCH AN ACTUAL TYPE THAT IS THE BASE TYPE OF
-- THE FORMAL PARAMETER'S DESIGNATED TYPE.  CHECK WHEN THE DESIGNATED
-- TYPE IS A RECORD TYPE AND IS NOT A GENERIC FORMAL TYPE DECLARED IN 
-- THE SAME FORMAL PART AS FT.

-- SPS 5/21/82
-- SPS 2/10/83

PROCEDURE BC3501E IS

     TYPE R1 IS RECORD NULL; END RECORD;
     TYPE AR1 IS ACCESS R1;
     GENERIC
          TYPE FT IS ACCESS R1;
     PACKAGE P IS END P;

     PACKAGE P1 IS NEW P (AR1);              -- OK.
     PACKAGE P2 IS NEW P (R1);               -- ERROR: R1 IS NOT AN
                                             -- ACCESS TYPE.

     TYPE ARR IS ARRAY (INTEGER RANGE 1 .. 1) OF CHARACTER;
     SUBTYPE INT IS INTEGER RANGE 1 .. 3;

     TYPE R2 IS
     RECORD
          C1 : INTEGER RANGE 1 .. 2;
          C2 : CHARACTER;
          C3 : ARR;
     END RECORD;
     TYPE AR2 IS ACCESS R2;
     GENERIC
          TYPE FT IS ACCESS R2;
     PACKAGE PA IS END PA;

     PACKAGE P3 IS NEW PA (R2);              -- ERROR: R2 IS NOT AN
                                             -- ACCESS TYPE.
     PACKAGE P4 IS NEW PA (R1);              -- ERROR: R1 IS NOT AN
                                             -- ACCESS TYPE.
     PACKAGE P5 IS NEW PA (AR2);             -- OK.

     TYPE R3 (D : INT := 1) IS
     RECORD
          CASE D IS
               WHEN 1 => NULL;
               WHEN OTHERS => NULL;
          END CASE;
     END RECORD;
     TYPE AR3 IS ACCESS R3;
     GENERIC
          TYPE FT IS ACCESS R3;
     PACKAGE PB IS END PB;

     PACKAGE P7 IS NEW PB (AR3);             -- OK.
     PACKAGE P8 IS NEW PB (R3);              -- ERROR: R3 IS NOT AN
                                             -- ACCESS TYPE.
BEGIN
     NULL;
END BC3501E;
