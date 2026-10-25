-- B48002B.ADA

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
-- CHECK THAT ILLEGAL FORMS OF ALLOCATORS ARE FORBIDDEN. IN PARTICULAR,
-- FOR ALLOCATORS OF THE FORM "NEW T X", WHERE X IS A DISCRIMINANT
-- CONSTRAINT OR A VALUE OF TYPE T ENCLOSED IN PARENTHESES, CHECK THAT T
-- CANNOT BE AN ACCESS TYPE WHOSE DESIGNATED TYPE IS:
--    A SCALAR TYPE,
--    A CONSTRAINED RECORD TYPE,
--    A CONSTRAINED PRIVATE TYPE,
--    A CONSTRAINED LIMITED TYPE,
--    AN ARRAY TYPE, OR
--    AN ACCESS TYPE (CONSTRAINED OR UNCONSTRAINED).

-- TYPES OF ERROR MESSAGES:
--   A - DISCRIMINANT CONSTRAINTS CANNOT BE APPLIED TO THIS TYPE.
--   B - CONSTRAINED TYPE CANNOT BE FURTHER CONSTRAINED.

-- EG  07/26/84

PROCEDURE B48002B IS

     PACKAGE P IS

          TYPE UP(DISC1, DISC2 : INTEGER) IS PRIVATE;
          TYPE UL(DISC1, DISC2 : INTEGER) IS LIMITED PRIVATE;

     PRIVATE

          TYPE UP(DISC1, DISC2 : INTEGER) IS
               RECORD
                    INT : INTEGER;
               END RECORD;
          TYPE UL(DISC1, DISC2 : INTEGER) IS
               RECORD
                    INT : INTEGER;
               END RECORD;

     END P;

     USE P;

     TYPE UR(DISC1, DISC2 : INTEGER) IS
          RECORD
               INT : INTEGER;
          END RECORD;
     SUBTYPE CP IS UP(10, 13);
     SUBTYPE CL IS UL(10, 13);
     SUBTYPE CR IS UR(2, 4);
     TYPE CA IS ARRAY(INTEGER RANGE 1 .. 5) OF INTEGER;

     TYPE A_INT IS ACCESS INTEGER;
     TYPE A_UR  IS ACCESS UR;
     TYPE A_CR  IS ACCESS CR;
     TYPE A_CP  IS ACCESS CP;
     TYPE A_CL  IS ACCESS CL;
     TYPE A_CA  IS ACCESS CA;
     TYPE A_AT1 IS ACCESS A_CR;
     TYPE A_AT2 IS ACCESS A_UR;

     TYPE AA_INT IS ACCESS A_INT;
     TYPE AA_CR  IS ACCESS A_CR;
     TYPE AA_CP  IS ACCESS A_CP;
     TYPE AA_CL  IS ACCESS A_CL;
     TYPE AA_CA  IS ACCESS A_CA;
     TYPE AA_AT1 IS ACCESS A_AT1;
     TYPE AA_AT2 IS ACCESS A_AT2;

     V_AA_INT : AA_INT;
     V_AA_CR  : AA_CR;
     V_AA_CP  : AA_CP;
     V_AA_CL  : AA_CL;
     V_AA_CA  : AA_CA;
     V_AA_AT1 : AA_AT1;
     V_AA_AT2 : AA_AT2;

BEGIN

     V_AA_INT := NEW A_INT(1, 2);            -- ERROR: A.
     V_AA_CR  := NEW A_CR(1, 2);             -- ERROR: B.
     V_AA_CP  := NEW A_CP(1, 2);             -- ERROR: B.
     V_AA_CL  := NEW A_CL(1, 2);             -- ERROR: B.
     V_AA_CA  := NEW A_CA(1, 2);             -- ERROR: A AND/OR B.
     V_AA_AT1 := NEW A_AT1(1, 2);            -- ERROR: A.
     V_AA_AT2 := NEW A_AT2(1, 2);            -- ERROR: A.

END B48002B;
