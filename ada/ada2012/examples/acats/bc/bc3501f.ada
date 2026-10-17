-- BC3501F.ADA

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
-- TYPE OF FT'S DESIGNATED TYPE.  CHECK WHEN THE DESIGNATED TYPE IS
-- A RECORD TYPE AND IS A GENERIC FORMAL TYPE DECLARED IN THE SAME 
-- FORMAL PART AS FT.

-- SPS 5/21/82
-- SPS 2/10/83

PROCEDURE BC3501F IS

     GENERIC
          TYPE T IS PRIVATE;
          TYPE FT IS ACCESS T;
     PACKAGE P IS END P;

     TYPE ARR IS ARRAY (BOOLEAN) OF NATURAL;

     TYPE R1 IS RECORD NULL; END RECORD;
     TYPE AR1 IS ACCESS R1;
     TYPE R2 IS 
     RECORD
          A : INTEGER RANGE -1 .. 0;
          B : BOOLEAN;
          C : ARR;
          D : R1;
          E : AR1;
     END RECORD;

     GENERIC
          TYPE T (D : NATURAL) IS PRIVATE;
          TYPE FT IS ACCESS T;
     PACKAGE PP IS END PP;

     TYPE AR2 IS ACCESS R2;
     TYPE R3 (D : NATURAL := 2) IS
     RECORD
          CASE D IS
               WHEN 1 .. 3 => NULL;
               WHEN OTHERS => NULL;
          END CASE;
     END RECORD;
     TYPE AR3 IS ACCESS R3;

     PACKAGE P1 IS NEW P (R1, AR1);          -- OK.
     PACKAGE P2 IS NEW P (R1, R1);           -- ERROR: R1 IS NOT AN
                                             -- ACCESS TYPE.
     PACKAGE P3 IS NEW P (R2, AR2);          -- OK.
     PACKAGE P4 IS NEW P (R2, R2);           -- ERROR: R2 IS NOT AN
                                             -- ACCESS TYPE.
     PACKAGE P5 IS NEW PP (R3, AR3);         -- OK.
     PACKAGE P6 IS NEW PP (R3, R3);          -- ERROR: R3 IS NOT AN
                                             -- ACCESS TYPE.
BEGIN
     NULL;
END BC3501F;
