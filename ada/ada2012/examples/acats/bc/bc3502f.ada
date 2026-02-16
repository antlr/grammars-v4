-- BC3502F.ADA

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
-- CHECK THAT WHEN A GENERIC FORMAL TYPE IS AN ACCESS TYPE,
-- THE FORMAL TYPE IS ONLY MATCHED WHEN ITS DESIGNATED BASE TYPE
-- IS THE SAME AS THE DESIGNATED BASE TYPE OF THE ACTUAL PARAMETER.

-- CHECK FOR WHEN THE DESIGNATED TYPE IS A GENERIC FORMAL TYPE DECLARED
-- IN THE SAME FORMAL PART AS FT.

-- SPS 5/25/82
-- JRL 11/14/95  Removed Ada95-incompatible cases (corresponding
--               designated subtypes must statically match).
-- PWN 03/28/96  Restored checks in Ada95 legal format.

PROCEDURE BC3502F IS

     SUBTYPE T IS INTEGER;
     TYPE U IS NEW INTEGER;
     TYPE A IS ACCESS T;
     TYPE AN IS ACCESS NATURAL;
     TYPE AI IS ACCESS INTEGER;
     TYPE AU IS ACCESS U;
     TYPE ENUM IS (E1, E2, E3, E4);
     TYPE EN IS (E1, E2, E3, E4);
     TYPE ENU IS NEW ENUM;
     SUBTYPE E IS ENUM;
     TYPE AENUM IS ACCESS ENUM;
     TYPE AEN IS ACCESS EN;
     TYPE AENU IS ACCESS ENU;
     TYPE AE IS ACCESS E;

     GENERIC
          TYPE T IS PRIVATE;
          TYPE FT IS ACCESS T;
     PACKAGE P IS END P;

     PACKAGE P1 IS NEW P (T, A);             -- OK.
     PACKAGE P2 IS NEW P (INTEGER, A);       -- OK.
     PACKAGE P3 IS NEW P (NATURAL, A);       -- ERROR: A.
     PACKAGE P4 IS NEW P (U, A);             -- ERROR: A.

     PACKAGE P5 IS NEW P (U, AU);            -- OK.
     PACKAGE P6 IS NEW P (INTEGER, AU);      -- ERROR: AU.
     PACKAGE P7 IS NEW P (NATURAL, AU);      -- ERROR: AU.
     PACKAGE P8 IS NEW P (T, AU);            -- ERROR: AU.
     PACKAGE P9 IS NEW P (INTEGER, AN);      -- ERROR: AN.
     PACKAGE P10 IS NEW P (NATURAL, AI);     -- ERROR: AI.
     PACKAGE P11 IS NEW P (T, AI);           -- OK.
     PACKAGE P12 IS NEW P (U, AI);           -- ERROR: AI.

     PACKAGE P13 IS NEW P (ENUM, AENUM);     -- OK.
     PACKAGE P14 IS NEW P (ENU, AENU);       -- OK.
     PACKAGE P15 IS NEW P (ENUM, AEN);       -- ERROR: AEN.
     PACKAGE P16 IS NEW P (ENUM, AE);        -- OK.
     PACKAGE P17 IS NEW P (ENUM, AENU);      -- ERROR: AENU.

BEGIN
     NULL;
END BC3502F;
