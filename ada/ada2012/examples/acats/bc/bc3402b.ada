-- BC3402B.ADA

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
-- CHECK THAT A FORMAL AND ACTUAL GENERIC ARRAY TYPE MUST HAVE THE SAME
-- NUMBER OF DIMENSIONS.

-- CHECK WHERE THE ACTUAL PARAMETER IS A FORMAL PARAMETER OF AN
-- ENCLOSING GENERIC UNIT.

-- SPS 6/8/82

PROCEDURE BC3402B IS
     SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;
     GENERIC
          TYPE ARR1 IS ARRAY (INTEGER) OF NATURAL;
          TYPE ARR2 IS ARRAY (INTEGER, INTEGER) OF NATURAL;
          TYPE ARR3 IS ARRAY (INTEGER, INTEGER, INTEGER) OF NATURAL;
     PACKAGE PACK IS

          GENERIC
               TYPE T IS ARRAY (INTEGER) OF NATURAL;
          PACKAGE P IS END P;

          GENERIC
               TYPE T IS ARRAY (INTEGER, INTEGER) OF NATURAL;
          PACKAGE PP IS END PP;

          GENERIC
               TYPE T IS ARRAY (INTEGER, INTEGER, INTEGER) OF NATURAL;
          PACKAGE PPP IS END PPP;

          PACKAGE P1 IS NEW P (ARR1);             -- OK.
          PACKAGE P2 IS NEW P (ARR2);             -- ERROR: ARR2.
          PACKAGE P3 IS NEW P (ARR3);             -- ERROR: ARR3.

          PACKAGE PP1 IS NEW PP (ARR1);           -- ERROR: ARR1.
          PACKAGE PP2 IS NEW PP (ARR2);           -- OK.
          PACKAGE PP3 IS NEW PP (ARR3);           -- ERROR: ARR3.

          PACKAGE PPP1 IS NEW PPP (ARR1);         -- ERROR: ARR1.
          PACKAGE PPP2 IS NEW PPP (ARR2);         -- ERROR: ARR2.
          PACKAGE PPP3 IS NEW PPP (ARR3);         -- OK.

     END PACK;

BEGIN
     NULL;
END BC3402B;
