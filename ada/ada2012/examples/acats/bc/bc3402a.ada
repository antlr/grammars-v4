-- BC3402A.ADA

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
-- CHECK THAT IN A GENERIC INSTANTIATION, A GENERIC ARRAY TYPE 
-- PARAMETER MUST HAVE THE SAME NUMBER OF INDICES (DIMENSIONS) AS THE
-- CORRESPONDING GENERIC FORMAL ARRAY TYPE PARAMETER.

-- ASL 9/3/81
-- SPS 7/19/82

PROCEDURE BC3402A IS

     TYPE INT IS RANGE 0 .. 1;

     TYPE ARR1A IS ARRAY(INT) OF INT;
     SUBTYPE ARR1B IS ARR1A;
     TYPE ARR2A IS ARRAY(INT,INT) OF INT;
     SUBTYPE ARR2B IS ARR2A;
     TYPE ARR3 IS ARRAY(INT,INT,INT) OF INT;

     TYPE UAR1A IS ARRAY(INT RANGE <>) OF INT;
     SUBTYPE UAR1B IS UAR1A;
     TYPE UAR2A IS ARRAY(INT RANGE <>,
                         INT RANGE <>) OF INT;
     SUBTYPE UAR2B IS UAR2A;
     TYPE UAR3 IS ARRAY(INT RANGE <>,INT RANGE <>,
                        INT RANGE <>) OF INT;

     GENERIC
          TYPE GFT1 IS ARRAY(INT) OF INT;
          TYPE GFT2 IS ARRAY(INT,INT) OF INT;
          TYPE GFT3 IS ARRAY(INT,INT,INT) OF INT;
     PACKAGE P IS END P;

     GENERIC
          TYPE T IS ARRAY(INT RANGE <>) OF INT;
          TYPE FT IS ARRAY(INT RANGE <>,INT RANGE <>) OF INT;
          TYPE GFT IS ARRAY(INT RANGE <>,INT RANGE <>,
                            INT RANGE <>) OF INT;
     PACKAGE PU IS END PU;

     PACKAGE OKP IS NEW P(ARR1A,ARR2A,ARR3);   -- OK.

     PACKAGE BAD1 IS NEW P(ARR2B,ARR2A,ARR3);  -- ERROR: ARR2B.
     PACKAGE BAD2 IS NEW P(ARR1A,ARR2A,ARR2B); -- ERROR: ARR2B.
     PACKAGE BAD3 IS NEW P(ARR1A,ARR1B,ARR3);  -- ERROR: ARR1B.

     PACKAGE OKPU IS NEW PU(UAR1A,UAR2A,UAR3); -- OK.

     PACKAGE PU1 IS NEW PU(UAR2B,UAR2A,UAR3);  -- ERROR: UAR2B.
     PACKAGE PU2 IS NEW PU(UAR1A,UAR2A,UAR2B); -- ERROR: UAR2B.
     PACKAGE PU3 IS NEW PU(UAR1A,UAR1B,UAR3);  -- ERROR: UAR1B.

BEGIN
     NULL;
END BC3402A;
