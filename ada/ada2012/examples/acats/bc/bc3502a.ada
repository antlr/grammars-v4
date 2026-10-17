-- BC3502A.ADA

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
-- IS THE SAME AS THE DESIGNATED TYPE OF THE ACTUAL PARAMETER.

-- CHECK FOR WHEN THE DESIGNATED TYPE IS NOT A GENERIC FORMAL TYPE.

-- SPS 5/24/82
-- JRL 11/30/95  Changed T to be a subtype of NATURAL to make package P1
--               OK case compatible with Ada95.

PROCEDURE BC3502A IS

     SUBTYPE T IS NATURAL;
     TYPE U IS NEW INTEGER;
     TYPE A IS ACCESS T;
     TYPE AU IS ACCESS U;
     GENERIC
          TYPE FT IS ACCESS NATURAL;
     PACKAGE P IS END P;

     PACKAGE P1 IS NEW P (A);                -- OK.
     PACKAGE P2 IS NEW P (AU);               -- ERROR: AU.

     TYPE ENUM IS (E1, E2, E3, E4);
     TYPE EN IS (E1, E2, E3, E4);
     TYPE ENU IS NEW ENUM;
     SUBTYPE E IS ENUM;
     TYPE AENUM IS ACCESS ENUM;
     TYPE AEN IS ACCESS EN;
     TYPE AENU IS ACCESS ENU;
     TYPE AE IS ACCESS E;

     GENERIC
          TYPE FT IS ACCESS ENUM;
     PACKAGE PP IS END PP;

     PACKAGE P3 IS NEW PP (AENUM);           -- OK.
     PACKAGE P4 IS NEW PP (AE);              -- OK.
     PACKAGE P5 IS NEW PP (AEN);             -- ERROR: AEN.
     PACKAGE P6 IS NEW PP (AU);              -- ERROR: AU.
     PACKAGE P7 IS NEW PP (AENU);            -- ERROR: AENU.

BEGIN
     NULL;
END BC3502A;
