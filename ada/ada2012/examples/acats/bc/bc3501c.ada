-- BC3501C.ADA

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
-- TYPE IS AN ARRAY TYPE AND IS NOT A GENERIC FORMAL TYPE DECLARED IN 
-- THE SAME FORMAL PART AS FT.

-- SPS 5/20/82

PROCEDURE BC3501C IS

     TYPE A1 IS ARRAY (INTEGER) OF INTEGER;
     TYPE AA1 IS ACCESS A1;
     GENERIC
          TYPE FT IS ACCESS A1;
     PACKAGE P IS END P;

     PACKAGE P1 IS NEW P (AA1);              -- OK.
     PACKAGE P2 IS NEW P (A1);               -- ERROR: A1 IS NOT AN
                                             -- ACCESS TYPE.

     TYPE A2 IS ARRAY (CHARACTER) OF INTEGER;
     TYPE AA2 IS ACCESS A2;
     GENERIC
          TYPE FT IS ACCESS A2;
     PACKAGE PA IS END PA;

     PACKAGE P3 IS NEW PA (AA2);             -- OK.
     PACKAGE P4 IS NEW PA (A2);              -- ERROR: A2 IS NOT AN
                                             -- ACCESS TYPE.

     TYPE A3 IS ARRAY (BOOLEAN) OF INTEGER;
     TYPE AA3 IS ACCESS A3;
     GENERIC
          TYPE FT IS ACCESS A3;
     PACKAGE PB IS END PB;

     PACKAGE P5 IS NEW PB (AA3);             -- OK.
     PACKAGE P6 IS NEW PB (A3);              -- ERROR: A3 IS NOT AN
                                             -- ACCESS TYPE.

     TYPE A4 IS ARRAY (INTEGER) OF CHARACTER;
     TYPE AA4 IS ACCESS A4;
     GENERIC
          TYPE FT IS ACCESS A4;
     PACKAGE PC IS END PC;

     PACKAGE P7 IS NEW PC (AA4);             -- OK.
     PACKAGE P8 IS NEW PC (A4);              -- ERROR: A4 IS NOT AN
                                             -- ACCESS TYPE.

     TYPE A5 IS ARRAY (INTEGER) OF BOOLEAN;
     TYPE AA5 IS ACCESS A5;
     GENERIC
          TYPE FT IS ACCESS A5;
     PACKAGE PD IS END PD;

     PACKAGE P9 IS NEW PD (AA5);             -- OK.
     PACKAGE P10 IS NEW PD (A5);             -- ERROR: A5 IS NOT AN
                                             -- ACCESS TYPE.

BEGIN
     NULL;
END BC3501C;
