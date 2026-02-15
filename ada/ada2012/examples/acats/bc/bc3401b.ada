-- BC3401B.ADA

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
-- CHECK THAT IF A GENERIC FORMAL TYPE IS AN ARRAY TYPE, THE ACTUAL
-- PARAMETER MUST ALSO BE AN ARRAY TYPE.

-- CHECK WHEN THE ACTUAL IS A GENERIC FORMAL PARAMETER APPEARING IN AN
-- ENCLOSING GENERIC DECLARATION.

-- SPS 6/8/82

PROCEDURE BC3401B IS

     SUBTYPE NATURAL IS INTEGER RANGE 1..INTEGER'LAST;

     GENERIC
          TYPE PV IS PRIVATE;
          TYPE LP IS LIMITED PRIVATE;
          TYPE INT IS RANGE <>;
          TYPE DESC IS (<>);
          TYPE ARR IS ARRAY (INTEGER) OF NATURAL;
     PACKAGE PACK IS

          GENERIC
               TYPE AR IS ARRAY (INTEGER) OF NATURAL;
          PACKAGE P IS END P;

          PACKAGE P1 IS NEW P (ARR);         -- OK.
          PACKAGE P2 IS NEW P (PV);          -- ERROR: PV.
          PACKAGE P3 IS NEW P (LP);          -- ERROR: LP.
          PACKAGE P4 IS NEW P (INT);         -- ERROR: INT.
          PACKAGE P5 IS NEW P (DESC);        -- ERROR: DESC.

     END PACK;

BEGIN
     NULL;
END BC3401B;
