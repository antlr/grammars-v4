-- BC3501G.ADA

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
-- TYPE IS A PRIVATE OR LIMITED PRIVATE TYPE AND IS NOT A GENERIC FORMAL
-- TYPE DECLARED IN THE SAME FORMAL PART AS FT.

-- SPS 5/21/82

PROCEDURE BC3501G IS

     PACKAGE PACK IS
          TYPE PV1 IS PRIVATE;
          TYPE PV2 IS PRIVATE;
          TYPE PV3 IS PRIVATE;
          TYPE PV4 IS PRIVATE;
          TYPE PV5 (D : INTEGER := 3) IS PRIVATE;
          TYPE LP1 IS LIMITED PRIVATE;
          TYPE LP2 IS LIMITED PRIVATE;
          TYPE LP3 IS LIMITED PRIVATE;
          TYPE LP4 IS LIMITED PRIVATE;

     PRIVATE
          TYPE PV1 IS NEW INTEGER RANGE 1 .. 1;
          TYPE LP1 IS NEW PV1;
          TYPE PV2 IS ARRAY (INTEGER RANGE 1 .. 1) OF CHARACTER;
          TYPE LP2 IS NEW PV2;
          TYPE PV3 IS 
          RECORD
               NULL;
          END RECORD;
          TYPE LP3 IS 
          RECORD
               C1 : INTEGER RANGE 1 .. 10;
               C2 : BOOLEAN;
          END RECORD;
          TYPE PV4 IS ACCESS PV1;
          TYPE LP4 IS NEW PV4;
          TYPE PV5 (D : INTEGER := 3) IS
          RECORD
               CASE D IS
                    WHEN 1 .. 5 => NULL;
                    WHEN OTHERS => NULL;
               END CASE;
          END RECORD;
     END PACK;

     USE PACK;

     GENERIC
          TYPE FT IS ACCESS PV1;
     PACKAGE PA IS END PA;

     GENERIC
          TYPE FT IS ACCESS LP1;
     PACKAGE PPA IS END PPA;

     TYPE APV1 IS ACCESS PV1;
     TYPE ALP1 IS ACCESS LP1;
     PACKAGE P1 IS NEW PA (APV1);  -- OK.
     PACKAGE P2 IS NEW PA (PV1);   -- ERROR: PV1 IS NOT AN
                                   -- ACCESS TYPE.
     PACKAGE P3 IS NEW PPA (ALP1); -- OK.
     PACKAGE P4 IS NEW PPA (LP1);  -- ERROR: LP1 IS NOT AN
                                   -- ACCESS TYPE.

     GENERIC
          TYPE FT IS ACCESS PV2;
     PACKAGE PB IS END PB;

     GENERIC
          TYPE FT IS ACCESS LP2;
     PACKAGE PPB IS END PPB;

     TYPE APV2 IS ACCESS PV2;
     TYPE ALP2 IS ACCESS LP2;
     PACKAGE P5 IS NEW PB (APV2);  -- OK.
     PACKAGE P6 IS NEW PB (PV2);   -- ERROR: PV2 IS NOT AN
                                   -- ACCESS TYPE.
     PACKAGE P7 IS NEW PPB (ALP2); -- OK.
     PACKAGE P8 IS NEW PPB (LP2);  -- ERROR: LP2 IS NOT AN
                                   -- ACCESS TYPE.

     GENERIC
          TYPE FT IS ACCESS PV3;
     PACKAGE PC IS END PC;

     GENERIC
          TYPE FT IS ACCESS LP3;
     PACKAGE PPC IS END PPC;

     TYPE APV3 IS ACCESS PV3;
     TYPE ALP3 IS ACCESS LP3;
     PACKAGE P9 IS NEW PC (APV3);  -- OK.
     PACKAGE P10 IS NEW PC (PV3);  -- ERROR: PV3 IS NOT AN
                                   -- ACCESS TYPE.
     PACKAGE P11 IS NEW PPC (ALP3);-- OK.
     PACKAGE P12 IS NEW PPC (LP3); -- ERROR: LP3 IS NOT AN
                                   -- ACCESS TYPE.

     GENERIC
          TYPE FT IS ACCESS PV4;
     PACKAGE PD IS END PD;

     GENERIC
          TYPE FT IS ACCESS LP4;
     PACKAGE PPD IS END PPD;

     TYPE APV4 IS ACCESS PV4;
     TYPE ALP4 IS ACCESS LP4;
     PACKAGE P13 IS NEW PD (APV4); -- OK.
     PACKAGE P14 IS NEW PD (PV4);  -- ERROR: PV4 IS NOT AN
                                   -- ACCESS TYPE.
     PACKAGE P15 IS NEW PPD (ALP4);-- OK.
     PACKAGE P16 IS NEW PPD (LP4); -- ERROR: LP4 IS NOT AN
                                   -- ACCESS TYPE.

     GENERIC
          TYPE FT IS ACCESS PV5;
     PACKAGE PE IS END PE;

     TYPE APV5 IS ACCESS PV5;

     PACKAGE P17 IS NEW PE (APV5); -- OK.
     PACKAGE P18 IS NEW PE (PV5);  -- ERROR: PV5 IS NOT AN
                                   -- ACCESS TYPE.

BEGIN
     NULL;
END BC3501G;
