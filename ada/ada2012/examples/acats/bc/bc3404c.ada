-- BC3404C.ADA

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
-- CHECK THAT THE COMPONENT BASE TYPE OF A GENERIC ARRAY TYPE PARAMETER
-- MUST BE THE SAME AS THE COMPONENT BASE TYPE OF THE ACTUAL PARAMETER.

-- CHECK WHEN THE COMPONENT TYPE OF THE FORMAL ARRAY TYPE IS A GENERIC 
-- FORMAL PARAMETER DECLARED IN AN ENCLOSING GENERIC UNIT.

-- SPS 6/23/82
-- JRK 3/26/84
-- JRL 11/14/95  Removed Ada95-incompatible cases (corresponding component
--               subtypes must statically match).
-- PWN 03/28/96  Restored checks in Ada95 legal format.

PROCEDURE BC3404C IS

     GENERIC
          TYPE COMP IS (<>);
     PACKAGE PACK IS
          SUBTYPE CC IS COMP RANGE COMP'FIRST .. COMP'VAL(2);
          TYPE NCOMP IS NEW COMP;
          SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;
          TYPE AR_COMP IS ARRAY (NATURAL) OF COMP;
          TYPE AR_CC IS ARRAY (NATURAL) OF CC;
          TYPE AR_B IS ARRAY (NATURAL) OF BOOLEAN;
          TYPE AR_NCOMP IS ARRAY (NATURAL) OF NCOMP;
          TYPE AR_I IS ARRAY (NATURAL) OF INTEGER;
          TYPE AR_C IS ARRAY (NATURAL) OF CHARACTER;

          GENERIC
               TYPE FT IS ARRAY (NATURAL) OF COMP;
          PACKAGE P IS END P;

          PACKAGE P1 IS NEW P (AR_COMP);     -- OK.
          PACKAGE P2 IS NEW P (AR_CC);       -- ERROR: AR_CC.
          PACKAGE P3 IS NEW P (AR_NCOMP);    -- ERROR: AR_NCOMP.
          PACKAGE P4 IS NEW P (AR_C);        -- ERROR: AR_C.
          PACKAGE P5 IS NEW P (AR_B);        -- ERROR: AR_B.
          PACKAGE P6 IS NEW P (AR_I);        -- ERROR: AR_I.

     END PACK;

BEGIN
     NULL;
END BC3404C;
