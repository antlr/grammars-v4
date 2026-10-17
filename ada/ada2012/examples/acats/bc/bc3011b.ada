-- BC3011B.ADA

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
-- CHECK THAT WHEN A GENERIC PACKAGE INSTANTIATION CONTAINS DECLARATIONS
-- OF SUBPROGRAMS WITH THE SAME SPECIFICATIONS, THE CALLS TO THE
-- SUBPROGRAMS ARE AMBIGIOUS OUTSIDE THE GENERIC BODY.

-- DAT 9/18/81
-- SPS 5/7/82

PROCEDURE BC3011B IS

     TYPE DINTEGER IS NEW INTEGER;

     GENERIC
          TYPE S IS PRIVATE;
          TYPE T IS PRIVATE;
     PACKAGE P1 IS
          PROCEDURE PR (X : S);
          PROCEDURE PR (X : T);
     END P1;

     PACKAGE BODY P1 IS
          PROCEDURE PR (X : S) IS 
          BEGIN
               NULL;
          END;

          PROCEDURE PR (X : T) IS 
          BEGIN 
               NULL; 
          END;
     END P1;

     PACKAGE I1 IS NEW P1 (INTEGER, CHARACTER); -- OK.
     PACKAGE I2 IS NEW P1 (INTEGER, DINTEGER);  -- OK.
     PACKAGE I3 IS NEW P1 (INTEGER, INTEGER);   -- OK.

BEGIN
     I1.PR(3);                                  -- OK.
     I1.PR('A');                                -- OK.
     I2.PR(3);                                  -- ERROR: 3 UNQUALIFIED
                                                -- IS AMBIGIOUS.
     I2.PR(INTEGER(3));                         -- OK.
     I2.PR(DINTEGER(3));                        -- OK.
     I3.PR(INTEGER(3));                         -- ERROR: AMBIGIOUS
                                                -- REFERENCE TO PR.
END BC3011B;
