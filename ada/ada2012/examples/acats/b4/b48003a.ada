-- B48003A.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT IF T IS:
--    1) A SCALAR TYPE,
--    2) AN ACCESS TYPE, OR
--    3) A PRIVATE TYPE,
-- (X) CANNOT BE AN AGGREGATE, WITH OR WITHOUT OTHERS.

-- KINDS OF ERROR MESSAGES:
--   A - AGGREGATES CANNOT BE WRITTEN FOR THIS TYPE.

-- EG  08/02/84
-- JRK 11/28/84

PROCEDURE B48003A IS

     PACKAGE P IS

          TYPE UP(DISC1, DISC2 : INTEGER) IS PRIVATE;

     PRIVATE

          TYPE UP(DISC1, DISC2 : INTEGER) IS
               RECORD
                    INT : INTEGER;
               END RECORD;

     END P;

     USE P;

     SUBTYPE CP IS UP(10, 13);

     TYPE A_INT IS ACCESS INTEGER;
     TYPE A_CP  IS ACCESS CP;
     TYPE A_UP  IS ACCESS UP;
     TYPE AA_CP IS ACCESS A_CP;
     TYPE AA_UP IS ACCESS A_UP;

     V_A_INT1, V_A_INT2 : A_INT;
     V_A_CP1,  V_A_CP2  : A_CP;
     V_AA_CP1, V_AA_CP2 : AA_CP;
     V_AA_UP1, V_AA_UP2 : AA_UP;

BEGIN

     V_A_INT1 := NEW INTEGER'(1, 2);             -- ERROR: A.
     V_A_INT2 := NEW INTEGER'(OTHERS => 0);      -- ERROR: A.
     V_A_CP1  := NEW CP'(1, 2);                  -- ERROR: A.
     V_A_CP2  := NEW CP'(2, OTHERS => 1);        -- ERROR: A.
     V_AA_CP1 := NEW A_CP'(1, 2);                -- ERROR: A.
     V_AA_CP2 := NEW A_CP'(2, 3, OTHERS => 4);   -- ERROR: A.
     V_AA_UP1 := NEW A_UP'(1, 2);                -- ERROR: A.
     V_AA_UP2 := NEW A_UP'(OTHERS => 1);         -- ERROR: A.

END B48003A;
