-- B74103E.ADA

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
-- CHECK THAT AN ACTUAL TYPE PARAMETER CORRESPONDING TO A GENERIC
-- FORMAL PRIVATE TYPE CANNOT BE A PRIVATE TYPE ( OR A SUBTYPE OF A
-- PRIVATE TYPE, OR A COMPOSITE TYPE WITH A SUBCOMPONENT OF A PRIVATE
-- TYPE OR SUBTYPE ) BEFORE ITS FULL DECLARATION.

-- DAT 9/18/81
-- SPS 4/27/82
-- BHS 6/18/84

PROCEDURE B74103E IS

     GENERIC
          TYPE T IS PRIVATE;
     PACKAGE G1 IS END G1; 

     GENERIC
          TYPE T IS LIMITED PRIVATE;
     PACKAGE G2 IS END G2;

     PACKAGE PKG IS

          TYPE PRIV IS PRIVATE;
          TYPE LPRIV IS LIMITED PRIVATE;

          SUBTYPE SP IS PRIV;
          SUBTYPE SLP IS LPRIV;

          TYPE REC1_PR IS RECORD
               C1 : PRIV;
          END RECORD;

          TYPE REC2_LP IS RECORD
               C2 : LPRIV;
          END RECORD;

          TYPE REC3_SP IS RECORD
               C3 : SP;
          END RECORD;

          TYPE REC4_SL IS RECORD
               C4 : SLP;
          END RECORD;

          TYPE ARR1_R1 IS ARRAY (1 .. 0) OF REC1_PR;
          TYPE ARR2_R2 IS ARRAY (1 .. 0) OF REC2_LP;
          TYPE ARR3_R3 IS ARRAY (1 .. 0) OF REC3_SP;
          TYPE ARR4_R4 IS ARRAY (1 .. 0) OF REC4_SL;

          SUBTYPE SR1 IS REC1_PR;
          SUBTYPE SR2 IS REC2_LP;
          SUBTYPE SR3 IS REC3_SP;
          SUBTYPE SR4 IS REC4_SL;
          SUBTYPE SA1 IS ARR1_R1;
          SUBTYPE SA2 IS ARR2_R2;
          SUBTYPE SA3 IS ARR3_R3;
          SUBTYPE SA4 IS ARR4_R4;

          GENERIC
          PACKAGE INNER IS
               PACKAGE I01 IS NEW G1 (PRIV);     -- ERROR: PRIV.
               PACKAGE I02 IS NEW G2 (PRIV);     -- ERROR: PRIV.
               PACKAGE I03 IS NEW G2 (LPRIV);    -- ERROR: LPRIV.
               PACKAGE I04 IS NEW G1 (SP);       -- ERROR: SP.
               PACKAGE I05 IS NEW G2 (SP);       -- ERROR: SP.
               PACKAGE I06 IS NEW G2 (SLP);      -- ERROR: SLP.
               PACKAGE I07 IS NEW G1 (REC1_PR);  -- ERROR: REC1_PR.
               PACKAGE I08 IS NEW G2 (REC1_PR);  -- ERROR: REC1_PR.
               PACKAGE I09 IS NEW G2 (REC2_LP);  -- ERROR: REC2_LP.
               PACKAGE I10 IS NEW G1 (REC3_SP);  -- ERROR: REC3_SP.
               PACKAGE I11 IS NEW G2 (REC3_SP);  -- ERROR: REC3_SP.
               PACKAGE I12 IS NEW G2 (REC4_SL);  -- ERROR: REC4_SL.
               PACKAGE I13 IS NEW G1 (ARR1_R1);  -- ERROR: ARR1_R1.
               PACKAGE I14 IS NEW G2 (ARR1_R1);  -- ERROR: ARR1_R1.
               PACKAGE I15 IS NEW G2 (ARR2_R2);  -- ERROR: ARR2_R2.
               PACKAGE I16 IS NEW G1 (ARR3_R3);  -- ERROR: ARR3_R3.
               PACKAGE I17 IS NEW G2 (ARR3_R3);  -- ERROR: ARR3_R3.
               PACKAGE I18 IS NEW G2 (ARR4_R4);  -- ERROR: ARR4_R4.
               PACKAGE I19 IS NEW G1 (SR1);      -- ERROR: SR1.
               PACKAGE I20 IS NEW G2 (SR1);      -- ERROR: SR1.
               PACKAGE I21 IS NEW G2 (SR2);      -- ERROR: SR2.
               PACKAGE I22 IS NEW G1 (SR3);      -- ERROR: SR3.
               PACKAGE I23 IS NEW G2 (SR3);      -- ERROR: SR3.
               PACKAGE I24 IS NEW G2 (SR4);      -- ERROR: SR4.
               PACKAGE I25 IS NEW G1 (SA1);      -- ERROR: SA1.
               PACKAGE I26 IS NEW G2 (SA1);      -- ERROR: SA1.
               PACKAGE I27 IS NEW G2 (SA2);      -- ERROR: SA2.
               PACKAGE I28 IS NEW G1 (SA3);      -- ERROR: SA3.
               PACKAGE I29 IS NEW G2 (SA3);      -- ERROR: SA3.
               PACKAGE I30 IS NEW G2 (SA4);      -- ERROR: SA4.
          END INNER;

          PACKAGE J01 IS NEW G1 (PRIV);     -- ERROR: PRIV.
          PACKAGE J02 IS NEW G2 (PRIV);     -- ERROR: PRIV.
          PACKAGE J03 IS NEW G2 (LPRIV);    -- ERROR: LPRIV.
          PACKAGE J04 IS NEW G1 (SP);       -- ERROR: SP.
          PACKAGE J05 IS NEW G2 (SP);       -- ERROR: SP.
          PACKAGE J06 IS NEW G2 (SLP);      -- ERROR: SLP.
          PACKAGE J07 IS NEW G1 (REC1_PR);  -- ERROR: REC1_PR.
          PACKAGE J08 IS NEW G2 (REC1_PR);  -- ERROR: REC1_PR.
          PACKAGE J09 IS NEW G2 (REC2_LP);  -- ERROR: REC2_LP.
          PACKAGE J10 IS NEW G1 (REC3_SP);  -- ERROR: REC3_SP.
          PACKAGE J11 IS NEW G2 (REC3_SP);  -- ERROR: REC3_SP.
          PACKAGE J12 IS NEW G2 (REC4_SL);  -- ERROR: REC4_SL.
          PACKAGE J13 IS NEW G1 (ARR1_R1);  -- ERROR: ARR1_R1.
          PACKAGE J14 IS NEW G2 (ARR1_R1);  -- ERROR: ARR1_R1.
          PACKAGE J15 IS NEW G2 (ARR2_R2);  -- ERROR: ARR2_R2.
          PACKAGE J16 IS NEW G1 (ARR3_R3);  -- ERROR: ARR3_R3.
          PACKAGE J17 IS NEW G2 (ARR3_R3);  -- ERROR: ARR3_R3.
          PACKAGE J18 IS NEW G2 (ARR4_R4);  -- ERROR: ARR4_R4.
          PACKAGE J19 IS NEW G1 (SR1);      -- ERROR: SR1.
          PACKAGE J20 IS NEW G2 (SR1);      -- ERROR: SR1.
          PACKAGE J21 IS NEW G2 (SR2);      -- ERROR: SR2.
          PACKAGE J22 IS NEW G1 (SR3);      -- ERROR: SR3.
          PACKAGE J23 IS NEW G2 (SR3);      -- ERROR: SR3.
          PACKAGE J24 IS NEW G2 (SR4);      -- ERROR: SR4.
          PACKAGE J25 IS NEW G1 (SA1);      -- ERROR: SA1.
          PACKAGE J26 IS NEW G2 (SA1);      -- ERROR: SA1.
          PACKAGE J27 IS NEW G2 (SA2);      -- ERROR: SA2.
          PACKAGE J28 IS NEW G1 (SA3);      -- ERROR: SA3.
          PACKAGE J29 IS NEW G2 (SA3);      -- ERROR: SA3.
          PACKAGE J30 IS NEW G2 (SA4);      -- ERROR: SA4.

     PRIVATE
          PACKAGE K01 IS NEW G1 (PRIV);     -- ERROR: PRIV.
          PACKAGE K02 IS NEW G2 (PRIV);     -- ERROR: PRIV.
          PACKAGE K03 IS NEW G2 (LPRIV);    -- ERROR: LPRIV.
          PACKAGE K04 IS NEW G1 (SP);       -- ERROR: SP.
          PACKAGE K05 IS NEW G2 (SP);       -- ERROR: SP.
          PACKAGE K06 IS NEW G2 (SLP);      -- ERROR: SLP.
          PACKAGE K07 IS NEW G1 (REC1_PR);  -- ERROR: REC1_PR.
          PACKAGE K08 IS NEW G2 (REC1_PR);  -- ERROR: REC1_PR.
          PACKAGE K09 IS NEW G2 (REC2_LP);  -- ERROR: REC2_LP.
          PACKAGE K10 IS NEW G1 (REC3_SP);  -- ERROR: REC3_SP.
          PACKAGE K11 IS NEW G2 (REC3_SP);  -- ERROR: REC3_SP.
          PACKAGE K12 IS NEW G2 (REC4_SL);  -- ERROR: REC4_SL.
          PACKAGE K13 IS NEW G1 (ARR1_R1);  -- ERROR: ARR1_R1.
          PACKAGE K14 IS NEW G2 (ARR1_R1);  -- ERROR: ARR1_R1.
          PACKAGE K15 IS NEW G2 (ARR2_R2);  -- ERROR: ARR2_R2.
          PACKAGE K16 IS NEW G1 (ARR3_R3);  -- ERROR: ARR3_R3.
          PACKAGE K17 IS NEW G2 (ARR3_R3);  -- ERROR: ARR3_R3.
          PACKAGE K18 IS NEW G2 (ARR4_R4);  -- ERROR: ARR4_R4.
          PACKAGE K19 IS NEW G1 (SR1);      -- ERROR: SR1.
          PACKAGE K20 IS NEW G2 (SR1);      -- ERROR: SR1.
          PACKAGE K21 IS NEW G2 (SR2);      -- ERROR: SR2.
          PACKAGE K22 IS NEW G1 (SR3);      -- ERROR: SR3.
          PACKAGE K23 IS NEW G2 (SR3);      -- ERROR: SR3.
          PACKAGE K24 IS NEW G2 (SR4);      -- ERROR: SR4.
          PACKAGE K25 IS NEW G1 (SA1);      -- ERROR: SA1.
          PACKAGE K26 IS NEW G2 (SA1);      -- ERROR: SA1.
          PACKAGE K27 IS NEW G2 (SA2);      -- ERROR: SA2.
          PACKAGE K28 IS NEW G1 (SA3);      -- ERROR: SA3.
          PACKAGE K29 IS NEW G2 (SA3);      -- ERROR: SA3.
          PACKAGE K30 IS NEW G2 (SA4);      -- ERROR: SA4.

          TYPE LPRIV IS (X);
          TYPE PRIV IS NEW LPRIV;

          PACKAGE OK01 IS NEW G1 (PRIV);     -- OK.
          PACKAGE OK02 IS NEW G2 (PRIV);     -- OK.
          PACKAGE OK03 IS NEW G2 (LPRIV);    -- OK.
          PACKAGE OK04 IS NEW G1 (SP);       -- OK.
          PACKAGE OK05 IS NEW G2 (SP);       -- OK.
          PACKAGE OK06 IS NEW G2 (SLP);      -- OK.
          PACKAGE OK07 IS NEW G1 (REC1_PR);  -- OK.
          PACKAGE OK08 IS NEW G2 (REC1_PR);  -- OK.
          PACKAGE OK09 IS NEW G2 (REC2_LP);  -- OK.
          PACKAGE OK10 IS NEW G1 (REC3_SP);  -- OK.
          PACKAGE OK11 IS NEW G2 (REC3_SP);  -- OK.
          PACKAGE OK12 IS NEW G2 (REC4_SL);  -- OK.
          PACKAGE OK13 IS NEW G1 (ARR1_R1);  -- OK.
          PACKAGE OK14 IS NEW G2 (ARR1_R1);  -- OK.
          PACKAGE OK15 IS NEW G2 (ARR2_R2);  -- OK.
          PACKAGE OK16 IS NEW G1 (ARR3_R3);  -- OK.
          PACKAGE OK17 IS NEW G2 (ARR3_R3);  -- OK.
          PACKAGE OK18 IS NEW G2 (ARR4_R4);  -- OK.
          PACKAGE OK19 IS NEW G1 (SR1);      -- OK.
          PACKAGE OK20 IS NEW G2 (SR1);      -- OK.
          PACKAGE OK21 IS NEW G2 (SR2);      -- OK.
          PACKAGE OK22 IS NEW G1 (SR3);      -- OK.
          PACKAGE OK23 IS NEW G2 (SR3);      -- OK.
          PACKAGE OK24 IS NEW G2 (SR4);      -- OK.
          PACKAGE OK25 IS NEW G1 (SA1);      -- OK.
          PACKAGE OK26 IS NEW G2 (SA1);      -- OK.
          PACKAGE OK27 IS NEW G2 (SA2);      -- OK.
          PACKAGE OK28 IS NEW G1 (SA3);      -- OK.
          PACKAGE OK29 IS NEW G2 (SA3);      -- OK.
          PACKAGE OK30 IS NEW G2 (SA4);      -- OK.
          PACKAGE OK31 IS NEW INNER;         -- OK.
     END;

BEGIN
     NULL;
END B74103E;
