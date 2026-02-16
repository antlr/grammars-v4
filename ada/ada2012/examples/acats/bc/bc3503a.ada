-- BC3503A.ADA

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
-- WHEN A GENERIC FORMAL TYPE FT IS AN ACCESS TYPE AND ITS
-- DESIGNATED TYPE T IS AN ARRAY TYPE OR A TYPE WITH DISCRIMINANTS
-- AND T IS MATCHED BY U, THE DESIGNATED TYPE OF THE ACTUAL
-- PARAMETER, CHECK THAT U MUST BE CONSTRAINED IF AND ONLY IF T IS
-- CONSTRAINED.

-- CHECK WHEN T IS NOT A GENERIC FORMAL TYPE.

-- SPS 5/27/82
-- JBG 11/8/85 AVOID CONFLICT WITH AI-7 OR AI-275
-- JRL 10/19/96 Removed cases under discussion in AI-00034.
-- EDS 12/01/97 Removed 3 error cases where actual designated types 
--              statically match.

PROCEDURE BC3503A IS

     SUBTYPE S3 IS STRING (1 .. 3);

     TYPE REC (D : CHARACTER) IS RECORD NULL; END RECORD;
     SUBTYPE CREC IS REC(D => 'A');

     PACKAGE PRIV IS
          TYPE PV (D : INTEGER := 3) IS PRIVATE;
--        SUBTYPE CPV IS PV (D => 2);
     PRIVATE
          TYPE PV (D : INTEGER := 3) IS
          RECORD
               CASE D IS
                    WHEN 1 .. 2 => C1 : INTEGER;
                    WHEN OTHERS => NULL;
               END CASE;
          END RECORD;
     END PRIV;

     USE PRIV;

     TYPE AS IS ACCESS STRING;
     SUBTYPE CAS IS AS (1 .. 3);
     TYPE AS3 IS ACCESS S3;
     TYPE ASC IS ACCESS STRING (1..3);

     TYPE AR IS ACCESS REC;
     SUBTYPE CAR IS AR (D => 'A');
     TYPE ACR IS ACCESS CREC;
     TYPE ARC IS ACCESS REC (D => 'A');

     TYPE APV IS ACCESS PV;
     SUBTYPE CAPV IS APV (D => 2);
     SUBTYPE CPV IS PRIV.PV(2);
     TYPE ACPV IS ACCESS CPV;
     TYPE APVC IS ACCESS PV (D => 2);

     GENERIC
          TYPE FT IS ACCESS STRING;
     PACKAGE PS IS END PS;

     GENERIC
          TYPE FT IS ACCESS S3;
     PACKAGE PCS IS END PCS;

     GENERIC
          TYPE FT IS ACCESS REC;
     PACKAGE PR IS END PR;

     GENERIC
          TYPE FT IS ACCESS CREC;
     PACKAGE PCR IS END PCR;

     GENERIC
          TYPE FT IS ACCESS PV;
     PACKAGE PP IS END PP;

     GENERIC
          TYPE FT IS ACCESS CPV;
     PACKAGE PCP IS END PCP;

     PACKAGE PS1 IS NEW PS (AS3);       -- ERROR: AS3 CONSTRAINED.
     PACKAGE PS2 IS NEW PS (ASC);       -- ERROR: ASC CONSTRAINED.
     PACKAGE PS3 IS NEW PS (CAS);       -- OK.
     PACKAGE PS4 IS NEW PS (AS);        -- OK.

     PACKAGE PCS1 IS NEW PCS (AS3);     -- OK.
     PACKAGE PCS2 IS NEW PCS (ASC);     -- OK.
     PACKAGE PCS4 IS NEW PCS (AS);      -- ERROR: AS NOT CONSTRAINED.

     PACKAGE PR1 IS NEW PR (ACR);       -- ERROR: ACR CONSTRAINED.
     PACKAGE PR2 IS NEW PR (ARC);       -- ERROR: ARC CONSTRAINED.
     PACKAGE PR3 IS NEW PR (CAR);       -- OK.
     PACKAGE PR4 IS NEW PR (AR);        -- OK.

     PACKAGE PCR1 IS NEW PCR (ACR);     -- OK.
     PACKAGE PCR2 IS NEW PCR (ARC);     -- OK.
     PACKAGE PCR4 IS NEW PCR (AR);      -- ERROR: AR NOT CONSTRAINED.

     PACKAGE PP1 IS NEW PP (ACPV);      -- ERROR: ACPV CONSTRAINED.
     PACKAGE PP2 IS NEW PP (APVC);      -- ERROR: APVC CONSTRAINED.
     PACKAGE PP3 IS NEW PP (CAPV);      -- OK.
     PACKAGE PP4 IS NEW PP (APV);       -- OK.

     PACKAGE PCP1 IS NEW PCP (ACPV);    -- OK.
     PACKAGE PCP2 IS NEW PCP (APVC);    -- OK.
     PACKAGE PCP4 IS NEW PCP (APV);     -- ERROR: PV NOT CONSTRAINED.

BEGIN
     NULL;
END BC3503A;
