-- BC3501A.ADA

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
-- TYPE IS NOT A GENERIC FORMAL TYPE DECLARED IN THE SAME FORMAL PART AS
-- FT.

-- DAT 9/24/81
-- SPS 5/18/82

PROCEDURE BC3501A IS


     TYPE AI IS ACCESS INTEGER;
     GENERIC
          TYPE FT IS ACCESS INTEGER;
     PACKAGE Q IS END Q;

     PACKAGE Q1 IS NEW Q (AI);               -- OK.
     PACKAGE Q2 IS NEW Q (INTEGER);          -- ERROR: INTEGER IS NOT AN
                                             -- ACCESS TYPE.

     TYPE AC IS ACCESS CHARACTER;
     GENERIC
          TYPE FT IS ACCESS CHARACTER;
     PACKAGE QA IS END QA;

     PACKAGE Q3 IS NEW QA (AC);              -- OK.
     PACKAGE Q4 IS NEW QA (CHARACTER);       -- ERROR: CHARACTER IS NOT
                                             -- AN ACCESS TYPE.

     TYPE AB IS ACCESS BOOLEAN;
     GENERIC
          TYPE FT IS ACCESS BOOLEAN;
     PACKAGE QB IS END QB;

     PACKAGE Q5 IS NEW QB (AB);              -- OK.
     PACKAGE Q6 IS NEW QB (BOOLEAN);         -- ERROR: BOOLEAN IS NOT AN
                                             -- ACCESS TYPE.

     TYPE ENUM IS (E1, E2, E3, E4);
     TYPE AE IS ACCESS ENUM;
     GENERIC
          TYPE FT IS ACCESS ENUM;
     PACKAGE QC IS END QC;

     PACKAGE Q7 IS NEW QC (AE);              -- OK.
     PACKAGE Q8 IS NEW QC (ENUM);            -- ERROR: ENUM IS NOT AN
                                             -- ACCESS TYPE.

BEGIN
     NULL;
END BC3501A;
