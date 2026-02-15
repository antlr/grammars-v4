-- BC3501H.ADA

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
-- A PRIVATE OR LIMITED PRIVATE TYPE AND IS A GENERIC FORMAL TYPE 
-- DECLARED IN THE SAME FORMAL PART AS FT.

-- SPS 5/21/82

PROCEDURE BC3501H IS

     PACKAGE PACK IS

          TYPE PV1 IS PRIVATE;
          TYPE LP1 IS LIMITED PRIVATE;
          TYPE PV2 IS PRIVATE;
          TYPE LP2 IS LIMITED PRIVATE;
          TYPE PV3 IS PRIVATE;
          TYPE LP3 IS LIMITED PRIVATE;
          TYPE PV4 IS PRIVATE;
          TYPE LP4 IS LIMITED PRIVATE;
          TYPE PV5 (D : INTEGER := 3) IS PRIVATE;
     PRIVATE
          TYPE PV1 IS NEW INTEGER RANGE 1 .. 10;
          TYPE PV2 IS ARRAY (INTEGER RANGE 1 .. 1) OF PV1;
          TYPE PV3 IS 
          RECORD
               C1 : INTEGER;
               C2 : PV2;
               C3 : PV1;
          END RECORD;
          TYPE PV4 IS ACCESS PV1;
          TYPE PV5 (D : INTEGER := 3) IS
          RECORD
               C1 : PV3;
               CASE D IS
                    WHEN 1 .. 10 => NULL;
                    WHEN OTHERS => C2 : PV4;
               END CASE;
          END RECORD;
          TYPE LP1 IS NEW PV1;
          TYPE LP2 IS NEW PV2;
          TYPE LP3 IS NEW PV3;
          TYPE LP4 IS NEW PV4;

     END PACK;

     USE PACK;

     GENERIC
          TYPE T IS LIMITED PRIVATE;
          TYPE FT IS ACCESS T;
     PACKAGE P IS END P;

     GENERIC 
          TYPE T (D : INTEGER) IS PRIVATE;
          TYPE FT IS ACCESS T;
     PACKAGE PP IS END PP;

     TYPE APV1 IS ACCESS PV1;
     TYPE ALP1 IS ACCESS LP1;
     TYPE APV2 IS ACCESS PV2;
     TYPE ALP2 IS ACCESS LP2;
     TYPE APV3 IS ACCESS PV3;
     TYPE ALP3 IS ACCESS LP3;
     TYPE APV4 IS ACCESS PV4;
     TYPE ALP4 IS ACCESS LP4;
     TYPE APV5 IS ACCESS PV5;

     PACKAGE P1 IS NEW P (PV1, APV1);        -- OK.
     PACKAGE P2 IS NEW P (LP1, ALP1);        -- OK.
     PACKAGE P3 IS NEW P (PV1, PV1);         -- ERROR: PV1 IS NOT AN
                                             -- ACCESS TYPE.
     PACKAGE P4 IS NEW P (LP1, LP1);         -- ERROR: LP1 IS NOT AN
                                             -- ACCESS TYPE.

     PACKAGE P5 IS NEW P (PV2, APV2);        -- OK.
     PACKAGE P6 IS NEW P (LP2, ALP2);        -- OK.
     PACKAGE P7 IS NEW P (PV2, PV2);         -- ERROR: PV2 IS NOT AN
                                             -- ACCESS TYPE.
     PACKAGE P8 IS NEW P (LP2, LP2);         -- ERROR: LP2 IS NOT AN
                                             -- ACCESS TYPE.

     PACKAGE P9 IS NEW P (PV3, APV3);        -- OK.
     PACKAGE P10 IS NEW P (LP3, ALP3);       -- OK.
     PACKAGE P11 IS NEW P (PV3, PV3);        -- ERROR: PV3 IS NOT AN
                                             -- ACCESS TYPE.
     PACKAGE P12 IS NEW P (LP3, LP3);        -- ERROR: LP3 IS NOT AN
                                             -- ACCESS TYPE.

     PACKAGE P13 IS NEW P (PV4, APV4);       -- OK.
     PACKAGE P14 IS NEW P (LP4, ALP4);       -- OK.
     PACKAGE P15 IS NEW P (PV4, PV4);        -- ERROR: PV4 IS NOT AN
                                             -- ACCESS TYPE.
     PACKAGE P16 IS NEW P (LP4, LP4);        -- ERROR: LP4 IS NOT AN
                                             -- ACCESS TYPE.

     PACKAGE P17 IS NEW PP (PV5, APV5);      -- OK.
     PACKAGE P18 IS NEW PP (PV5, PV5);       -- ERROR: PR5 IS NOT AN
                                             -- ACCESS TYPE.

BEGIN
     NULL;
END BC3501H;
