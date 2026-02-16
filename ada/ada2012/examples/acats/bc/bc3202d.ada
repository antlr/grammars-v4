-- BC3202D.ADA

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
-- CHECK THAT IF A GENERIC FORMAL PRIVATE TYPE HAS A DISCRIMINANT
-- PART, THEN AN ACTUAL TYPE PARAMETER MUST HAVE THE SAME DISCRIMINANT
-- PART.  THE NUMBER, ORDER, AND BASE TYPES OF DISCRIMINANTS
-- MUST BE THE SAME.

-- DAT 9/18/81
-- SPS 7/21/82
-- JWC 6/28/85   RENAMED FROM BC3203B-B.ADA
-- LDC 8/19/88   CHANGED ABBREVIATION DESC (FOR DISCRIMINANT) TO DISC
--               THROUGHOUT.
-- JRL 11/30/95  Removed Ada95-incompatible cases (corresponding discriminant
--               subtypes must statically match).
-- PWN 03/28/96  Restored checks in Ada95 legal format.

PROCEDURE BC3202D IS

     SUBTYPE SC IS CHARACTER;
     SUBTYPE SI IS INTEGER RANGE 1 .. 10;
     TYPE DC IS NEW CHARACTER;
     TYPE DI IS NEW INTEGER RANGE 1 .. 10;
     C1 : CONSTANT INTEGER := 1;
     C2 : CONSTANT INTEGER := 10;

     TYPE REC1 (A, B: CHARACTER; C: INTEGER; D:DI) IS
              RECORD NULL; END RECORD;

     TYPE REC2 (D1 : CHARACTER; D2 : DC; D3 : INTEGER; D4 : DI) IS
          RECORD
               C13 : STRING (D3 .. 12);
          END RECORD;

     TYPE REC3 (D2, D1 : CHARACTER; D3 : INTEGER; D4 : DI) IS
          RECORD NULL; END RECORD;

     TYPE REC4 (D1 : CHARACTER; D3 : INTEGER; D4 : DI) IS
          RECORD NULL; END RECORD;

     TYPE REC5 IS
          RECORD
               D1 , D2 : CHARACTER;
               D3 : INTEGER;
               D4 : DI;
          END RECORD;

     TYPE REC6 (D1, D2 : CHARACTER := 'A'; D3 : INTEGER := 5;
               D4 : DI := 1) IS
          RECORD NULL; END RECORD;

     TYPE REC7 (D1, D2: SC; D3: SI; D4 : DI) IS
          RECORD NULL; END RECORD;
 
     TYPE REC8 (A, B: CHARACTER; D3: INTEGER; D4 :SI) IS
          RECORD NULL; END RECORD;

     GENERIC
          TYPE T (D1, D2 : CHARACTER; D3 : INTEGER; D4 : DI) IS
               PRIVATE;
     PACKAGE G1 IS END G1;

     GENERIC
          TYPE T (D1 : CHARACTER; D2 : CHARACTER;
               D3 : INTEGER; D4 : DI) IS LIMITED PRIVATE;
     PACKAGE G2 IS END G2;

     PACKAGE NG1 IS NEW G1 (REC1); -- OK.
     PACKAGE NG2 IS NEW G1 (REC2); -- ERROR: DISC TYPE MISMATCH.
     PACKAGE NG3 IS NEW G1 (REC3); -- OK.
     PACKAGE NG4 IS NEW G1 (REC4); -- ERROR: WRONG NUMBER OF DISC.
     PACKAGE NG5 IS NEW G1 (REC5); -- ERROR: WRONG NUMBER OF DISC.
     PACKAGE NG6 IS NEW G1 (REC6); -- OK.
     PACKAGE NG7 IS NEW G1 (REC7); -- ERROR: REC7 MUST STATICLY MATCH.
     PACKAGE NG8 IS NEW G1 (REC8); -- ERROR: DISC TYPE MISMATCH.

     PACKAGE NP1 IS NEW G2 (REC1); -- OK.
     PACKAGE NP2 IS NEW G2 (REC2); -- ERROR: DISC TYPE MISMATCH.
     PACKAGE NP3 IS NEW G2 (REC3); -- OK.
     PACKAGE NP4 IS NEW G2 (REC4); -- ERROR: WRONG NUMBER OF DISC.
     PACKAGE NP5 IS NEW G2 (REC5); -- ERROR: WRONG NUMBER OF DISC.
     PACKAGE NP6 IS NEW G2 (REC6); -- OK.
     PACKAGE NP7 IS NEW G2 (REC7); -- ERROR: REC7 MUST STATICLY MATCH.
     PACKAGE NP8 IS NEW G2 (REC8); -- ERROR: DISC TYPE MISMATCH.

BEGIN
     NULL;
END BC3202D;
