-- BC3503E.ADA

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
-- WHEN A GENERIC FORMAL TYPE FT IS AN ACCESS TYPE AND ITS DESIGNATED
-- TYPE T IS AN ACCESS TYPE TO AN ARRAY TYPE OR A TYPE WITH
-- DISCRIMINANTS, AND T IS MATCHED BY U, THE DESIGNATED TYPE OF THE
-- ACTUAL PARAMETER, CHECK THAT U MUST BE CONSTRAINED IF AND ONLY IF T 
-- IS CONSTRAINED.

-- CHECK WHEN T IS A GENERIC FORMAL ACCESS TYPE DECLARED IN THE SAME 
-- FORMAL PART AS FT.

-- SPS 6/2/82

PROCEDURE BC3503E IS

     TYPE REC (D : INTEGER) IS RECORD NULL; END RECORD;
     TYPE AS IS ACCESS STRING;
     SUBTYPE CAS IS AS (1 .. 10);
     TYPE AREC IS ACCESS REC;
     SUBTYPE CAREC IS AREC (D => 10);

     TYPE AAS IS ACCESS AS;
     TYPE ACAS IS ACCESS CAS;
     TYPE AASC IS ACCESS AS (1 .. 10);

     TYPE AAREC IS ACCESS AREC;
     TYPE AARECC IS ACCESS AREC (D => 10);
     TYPE ACAREC IS ACCESS CAREC;

     GENERIC
          TYPE T IS PRIVATE;
          TYPE FT IS ACCESS T;
     PACKAGE P IS END P;

     PACKAGE PCAS1 IS NEW P (CAS, AASC);          -- OK.
     PACKAGE PCAS2 IS NEW P (CAS, ACAS);          -- OK.
     PACKAGE PCAS3 IS NEW P (CAS, AAS);           -- ERROR: AAS.

     PACKAGE PAS1 IS NEW P (AS, AAS);             -- OK.
     PACKAGE PAS2 IS NEW P (AS, AASC);            -- ERROR: AASC.
     PACKAGE PAS3 IS NEW P (AS, ACAS);            -- ERROR: ACAS.

     PACKAGE PCAREC1 IS NEW P (CAREC, AARECC);    -- OK.
     PACKAGE PCAREC2 IS NEW P (CAREC, ACAREC);    -- OK.
     PACKAGE PCAREC3 IS NEW P (CAREC, AAREC);     -- ERROR: AAREC.

     PACKAGE PAREC1 IS NEW P (AREC, AARECC);      -- ERROR: AARECC.
     PACKAGE PAREC2 IS NEW P (AREC, ACAREC);      -- ERROR: ACAREC.
     PACKAGE PAREC3 IS NEW P (AREC, AAREC);       -- OK.

BEGIN
     NULL;
END BC3503E; 
