-- B38105B.ADA

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
-- CHECK THAT AN ACTUAL TYPE PARAMETER CORRESPONDING TO A PRIVATE
-- OR LIMITED PRIVATE GENERIC FORMAL TYPE PARAMETER CANNOT
-- BE AN INCOMPLETE TYPE.

-- DAT 9/18/81
-- SPS 2/10/83
-- JBG 11/08/85 AVOID CONFLICT WITH AI-7 OR AI-275

PROCEDURE B38105B IS

     GENERIC
          TYPE T IS PRIVATE;
     PACKAGE G1 IS END G1;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
     PACKAGE G2 IS END G2;

     GENERIC
          TYPE T (B : BOOLEAN) IS LIMITED PRIVATE;
     PACKAGE G3 IS END G3;

     TYPE I1;
     TYPE AI IS ACCESS I1;

     TYPE I2 (B : BOOLEAN);
     TYPE AI2 IS ACCESS I2;
--   TYPE AI2T IS ACCESS I2 (TRUE);

     PACKAGE P1 IS NEW G1 (I1);               -- ERROR: I1.
     PACKAGE P2 IS NEW G1 (AI);               -- OK.
     PACKAGE P4 IS NEW G1 (I2);               -- ERROR: I2.
     PACKAGE P6 IS NEW G3 (I2);               -- ERROR: I2.
     PACKAGE P7 IS NEW G3 (AI2);              -- ERROR: AI2.
     PACKAGE P9 IS NEW G1 (AI2);              -- OK.
--   PACKAGE P10 IS NEW G1 (AI2T);            -- OK.
     PACKAGE P11 IS NEW G2 (I1);              -- ERROR: I1.
     PACKAGE P12 IS NEW G2 (AI);              -- OK.
     PACKAGE P13 IS NEW G2 (I2);              -- ERROR: I2.
     PACKAGE P15 IS NEW G2 (AI2);             -- OK.
--   PACKAGE P16 IS NEW G2 (AI2T);            -- OK.

     TYPE I1 IS (X);
     TYPE I2 (B : BOOLEAN) IS
          RECORD NULL; END RECORD;

BEGIN
     NULL;
END B38105B;
