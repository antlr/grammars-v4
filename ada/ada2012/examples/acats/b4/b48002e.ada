-- B48002E.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T X", WHERE X IS AN INDEX CONSTRAINT,
-- CHECK THAT T CANNOT BE AN ACCESS TYPE WHOSE DESIGNATED TYPE IS A
-- SCALAR, RECORD, PRIVATE, LIMITED PRIVATE, OR ACCESS TYPE, NOR CAN
-- THE DESIGNATED TYPE BE A CONSTRAINED ARRAY TYPE.

-- TYPES OF ERROR MESSAGES:
--   A - INDEX CONSTRAINTS CANNOT BE APPLIED TO THIS TYPE.
--   B - CONSTRAINED TYPE CANNOT BE FURTHER CONSTRAINED.

-- EG  07/27/84

PROCEDURE B48002E IS

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
     TYPE UA IS ARRAY(INTEGER RANGE <>) OF INTEGER;

     SUBTYPE CP IS UP(10, 13);
     SUBTYPE CL IS UL(10, 13);
     SUBTYPE CR IS UR(2, 4);
     SUBTYPE CA IS UA(1 .. 5);

     TYPE A_INT IS ACCESS INTEGER;
     TYPE A_UR  IS ACCESS UR;
     TYPE A_CR  IS ACCESS CR;
     TYPE A_UP  IS ACCESS UP;
     TYPE A_CP  IS ACCESS CP;
     TYPE A_UL  IS ACCESS UL;
     TYPE A_CL  IS ACCESS CL;
     TYPE A_UA  IS ACCESS UA;
     TYPE A_CA  IS ACCESS CA;
     TYPE A_AT1 IS ACCESS A_UA;
     TYPE A_AT2 IS ACCESS A_CA;

     TYPE AA_INT IS ACCESS A_INT;
     TYPE AA_UR  IS ACCESS A_UR;
     TYPE AA_CR  IS ACCESS A_CR;
     TYPE AA_UP  IS ACCESS A_UP;
     TYPE AA_CP  IS ACCESS A_CP;
     TYPE AA_UL  IS ACCESS A_UL;
     TYPE AA_CL  IS ACCESS A_CL;
     TYPE AA_CA  IS ACCESS A_CA;
     TYPE AA_AT1 IS ACCESS A_AT1;
     TYPE AA_AT2 IS ACCESS A_AT2;

     V_AA_INT : AA_INT;
     V_AA_UR  : AA_UR;
     V_AA_CR  : AA_CR;
     V_AA_UP  : AA_UP;
     V_AA_CP  : AA_CP;
     V_AA_UL  : AA_UL;
     V_AA_CL  : AA_CL;
     V_AA_CA  : AA_CA;
     V_AA_AT1 : AA_AT1;
     V_AA_AT2 : AA_AT2;

BEGIN

     V_AA_INT := NEW A_INT(1 .. 3);          -- ERROR: A.
     V_AA_UR  := NEW A_UR(1 .. 3);           -- ERROR: A.
     V_AA_CR  := NEW A_CR(1 .. 3);           -- ERROR: A.
     V_AA_UP  := NEW A_UP(1 .. 3);           -- ERROR: A.
     V_AA_CP  := NEW A_CP(1 .. 3);           -- ERROR: A.
     V_AA_UL  := NEW A_UL(1 .. 3);           -- ERROR: A.
     V_AA_CL  := NEW A_CL(1 .. 3);           -- ERROR: A.
     V_AA_CA  := NEW A_CA(1 .. 3);           -- ERROR: B.
     V_AA_AT1 := NEW A_AT1(1 .. 3);          -- ERROR: A.
     V_AA_AT2 := NEW A_AT2(1 .. 3);          -- ERROR: A.

END B48002E;
