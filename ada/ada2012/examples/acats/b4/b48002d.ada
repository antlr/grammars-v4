-- B48002D.ADA

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
-- CONSTRAINT OR A VALUE OF TYPE T ENCLOSED IN PARENTHESES, CHECK THAT
-- IF T IS: 
--   A) AN UNCONSTRAINED RECORD, A PRIVATE, OR LIMITED PRIVATE TYPE 
--      WITH DISCRIMINANTS, OR
--   B) AN UNCONSTRAINED ACCESS TYPE WHOSE DESIGNATED TYPE IS AN
--      UNCONSTRAINED RECORD, PRIVATE, OR LIMITED PRIVATE TYPE WITH
--      DISCRIMINANTS,
-- THEN X MUST BE A LEGAL DISCRIMINANT CONSTRAINT FOR T, NAMELY:
--   1 - THE DISCRIMINANT_NAMES GIVEN IN THE CONSTRAINT CANNOT BE 
--       DIFFERENT FROM THE NAMES OF THE DISCRIMINANTS OF THE TYPE BEING
--       CONSTRAINED. 
--   2 - THE SAME NAME CANNOT APPEAR TWICE AS A DISCRIMINANT_NAME IN A
--       PARTICULAR DISCRIMINANT_ASSOCIATION OR IN DIFFERENT 
--       DISCRIMINANT ASSOCIATIONS OF THE SAME DISCRIMINANT CONSTRAINT.
--   3 - IF A MIXTURE OF NAMED AND POSITIONAL ASSOCIATION IS USED, A 
--       NAMED DISCRIMINANT ASSOCIATION CANNOT GIVE A VALUE FOR A 
--       DISCRIMINANT WHOSE VALUE HAS ALREADY BEEN SPECIFIED 
--       POSITIONALLY.
--   4 - TOO MANY OR TOO FEW DISCRIMINANT VALUES CANNOT BE GIVEN.
--   5 - UNNAMED (I.E., POSITIONAL) DISCRIMINANT VALUES CANNOT BE GIVEN 
--       AFTER A DISCRIMINANT ASSOCIATION USING DISCRIMINANT NAMES.
--   6 - THE BASE TYPE OF THE SPECIFIED DISCRIMINANT VALUE CANNOT BE 
--       DIFFERENT FROM THE BASE TYPE OF THE CORRESPONDING DISCRIMINANT.
--   7 - OTHERS CANNOT BE USED AS A DISCRIMINANT NAME.

-- EG  07/26/84

PROCEDURE B48002D IS

     PACKAGE P IS

          SUBTYPE INT_TYPE1 IS INTEGER RANGE 1 .. 12;
          TYPE INT_TYPE2 IS (A, B, C, D, E);

          TYPE UP(A, B, C : INT_TYPE1) IS PRIVATE;
          TYPE UL(A, B, C : INT_TYPE1) IS LIMITED PRIVATE;

          INT2 : INT_TYPE2 := C;

     PRIVATE

          TYPE UP(A, B, C : INT_TYPE1) IS
               RECORD
                    INT : INTEGER;
               END RECORD;
          TYPE UL(A, B, C : INT_TYPE1) IS
               RECORD
                    INT : INTEGER;
               END RECORD;

     END P;

     USE P;

     TYPE UR(A, B, C : INT_TYPE1) IS
          RECORD
               INT : INTEGER;
          END RECORD;


     TYPE A_UR IS ACCESS UR;
     TYPE A_UP IS ACCESS UP;
     TYPE A_UL IS ACCESS UL;

BEGIN

     -- UNCONSTRAINED RECORD TYPE (WITH AT LEAST ONE DISCRIMINANT)

     DECLARE

          VUR0, VUR1, VUR2, VUR3, VUR4, VUR5, VUR6, VUR7,
          VUR8, VUR9, VUR10, VUR11, VUR12, VUR13, VUR14 : A_UR;

     BEGIN

          VUR0  := NEW UR(B => 1, C => 2, D => 3);     -- ERROR: 1.
          VUR1  := NEW UR(A | B | A => 1);             -- ERROR: 2.
          VUR2  := NEW UR(A => 1, B => 2, A => 3);     -- ERROR: 2.
          VUR3  := NEW UR(A => 1, B => 2, A => 1);     -- ERROR: 2.
          VUR4  := NEW UR(2, C => 3, A => 2);          -- ERROR: 3.
          VUR5  := NEW UR(1, 2, A => 1);               -- ERROR: 3.
          VUR6  := NEW UR(A => 1, B => 2);             -- ERROR: 4.
          VUR7  := NEW UR(A => 1, C => 3);             -- ERROR: 4.
          VUR8  := NEW UR(1);                          -- ERROR: 4.
          VUR9  := NEW UR(1, 2, 3, 4);                 -- ERROR: 4.
          VUR10 := NEW UR(A => 1, B => 2, 3);          -- ERROR: 5.
          VUR11 := NEW UR(A => 1, 2, C => 3);          -- ERROR: 5.
          VUR12 := NEW UR(2, INT2, 7);                 -- ERROR: 6.
          VUR13 := NEW UR(INT2, 4, 6);                 -- ERROR: 6.
          VUR14 := NEW UR(OTHERS => 1);                -- ERROR: 7.

     END;

     -- UNCONSTRAINED PRIVATE TYPE (WITH AT LEAST ONE DISCRIMINANT)

     DECLARE

          VUP0, VUP1, VUP2, VUP3, VUP4, VUP5, VUP6, VUP7,
          VUP8, VUP9, VUP10, VUP11, VUP12, VUP13, VUP14 : A_UP;

     BEGIN

          VUP0  := NEW UP(B => 1, C => 2, D => 3);     -- ERROR: 1.
          VUP1  := NEW UP(A | B | A => 1);             -- ERROR: 2.
          VUP2  := NEW UP(A => 1, B => 2, A => 3);     -- ERROR: 2.
          VUP3  := NEW UP(A => 1, B => 2, A => 1);     -- ERROR: 2.
          VUP4  := NEW UP(2, C => 3, A => 2);          -- ERROR: 3.
          VUP5  := NEW UP(1, 2, A => 1);               -- ERROR: 3.
          VUP6  := NEW UP(A => 1, B => 2);             -- ERROR: 4.
          VUP7  := NEW UP(A => 1, C => 3);             -- ERROR: 4.
          VUP8  := NEW UP(1);                          -- ERROR: 4.
          VUP9  := NEW UP(1, 2, 3, 4);                 -- ERROR: 4.
          VUP10 := NEW UP(A => 1, B => 2, 3);          -- ERROR: 5.
          VUP11 := NEW UP(A => 1, 2, C => 3);          -- ERROR: 5.
          VUP12 := NEW UP(2, INT2, 7);                 -- ERROR: 6.
          VUP13 := NEW UP(INT2, 4, 6);                 -- ERROR: 6.
          VUP14 := NEW UP(OTHERS => 1);                -- ERROR: 7.

     END;

     -- UNCONSTRAINED LIMITED TYPE (WITH AT LEAST ONE DISCRIMINANT)

     DECLARE

          VUL0, VUL1, VUL2, VUL3, VUL4, VUL5, VUL6, VUL7,
          VUL8, VUL9, VUL10, VUL11, VUL12, VUL13, VUL14 : A_UL;

     BEGIN

          VUL0  := NEW UL(B => 1, C => 2, D => 3);     -- ERROR: 1.
          VUL1  := NEW UL(A | B | A => 1);             -- ERROR: 2.
          VUL2  := NEW UL(A => 1, B => 2, A => 3);     -- ERROR: 2.
          VUL3  := NEW UL(A => 1, B => 2, A => 1);     -- ERROR: 2.
          VUL4  := NEW UL(2, C => 3, A => 2);          -- ERROR: 3.
          VUL5  := NEW UL(1, 2, A => 1);               -- ERROR: 3.
          VUL6  := NEW UL(A => 1, B => 2);             -- ERROR: 4.
          VUL7  := NEW UL(A => 1, C => 3);             -- ERROR: 4.
          VUL8  := NEW UL(1);                          -- ERROR: 4.
          VUL9  := NEW UL(1, 2, 3, 4);                 -- ERROR: 4.
          VUL10 := NEW UL(A => 1, B => 2, 3);          -- ERROR: 5.
          VUL11 := NEW UL(A => 1, 2, C => 3);          -- ERROR: 5.
          VUL12 := NEW UL(2, INT2, 7);                 -- ERROR: 6.
          VUL13 := NEW UL(INT2, 4, 6);                 -- ERROR: 6.
          VUL14 := NEW UL(OTHERS => 1);                -- ERROR: 7.

     END;

     -- UNCONSTRAINED ACCESS TYPE.

     DECLARE

          TYPE AA_UR IS ACCESS A_UR;
          TYPE AA_UP IS ACCESS A_UP;
          TYPE AA_UL IS ACCESS A_UL;

          V_AA_UR0,  V_AA_UR1,  V_AA_UR2,  V_AA_UR3, V_AA_UR4, V_AA_UR5,
          V_AA_UR6,  V_AA_UR7,  V_AA_UR8,  V_AA_UR9, V_AA_UR10,
          V_AA_UR11, V_AA_UR12, V_AA_UR13, V_AA_UR14          : AA_UR;
          V_AA_UP0,  V_AA_UP1,  V_AA_UP2,  V_AA_UP3, V_AA_UP4, V_AA_UP5,
          V_AA_UP6,  V_AA_UP7,  V_AA_UP8,  V_AA_UP9, V_AA_UP10,
          V_AA_UP11, V_AA_UP12, V_AA_UP13, V_AA_UP14          : AA_UP;
          V_AA_UL0,  V_AA_UL1,  V_AA_UL2,  V_AA_UL3, V_AA_UL4, V_AA_UL5,
          V_AA_UL6,  V_AA_UL7,  V_AA_UL8,  V_AA_UL9, V_AA_UL10,
          V_AA_UL11, V_AA_UL12, V_AA_UL13, V_AA_UL14          : AA_UL;

     BEGIN

          V_AA_UR0  := NEW A_UR(B => 1, C => 2, D => 3);     -- ERROR: 1
          V_AA_UR1  := NEW A_UR(A | B | A => 1);             -- ERROR: 2
          V_AA_UR2  := NEW A_UR(A => 1, B => 2, A => 3);     -- ERROR: 2
          V_AA_UR3  := NEW A_UR(A => 1, B => 2, A => 1);     -- ERROR: 2
          V_AA_UR4  := NEW A_UR(2, C => 3, A => 2);          -- ERROR: 3
          V_AA_UR5  := NEW A_UR(1, 2, A => 1);               -- ERROR: 3
          V_AA_UR6  := NEW A_UR(A => 1, B => 2);             -- ERROR: 4
          V_AA_UR7  := NEW A_UR(A => 1, C => 3);             -- ERROR: 4
          V_AA_UR8  := NEW A_UR(1);                          -- ERROR: 4
          V_AA_UR9  := NEW A_UR(1, 2, 3, 4);                 -- ERROR: 4
          V_AA_UR10 := NEW A_UR(A => 1, B => 2, 3);          -- ERROR: 5
          V_AA_UR11 := NEW A_UR(A => 1, 2, C => 3);          -- ERROR: 5
          V_AA_UR12 := NEW A_UR(2, INT2, 7);                 -- ERROR: 6
          V_AA_UR13 := NEW A_UR(INT2, 4, 6);                 -- ERROR: 6
          V_AA_UR14 := NEW A_UR(OTHERS => 1);                -- ERROR: 7

          V_AA_UP0  := NEW A_UP(B => 1, C => 2, D => 3);     -- ERROR: 1
          V_AA_UP1  := NEW A_UP(A | B | A => 1);             -- ERROR: 2
          V_AA_UP2  := NEW A_UP(A => 1, B => 2, A => 3);     -- ERROR: 2
          V_AA_UP3  := NEW A_UP(A => 1, B => 2, A => 1);     -- ERROR: 2
          V_AA_UP4  := NEW A_UP(2, C => 3, A => 2);          -- ERROR: 3
          V_AA_UP5  := NEW A_UP(1, 2, A => 1);               -- ERROR: 3
          V_AA_UP6  := NEW A_UP(A => 1, B => 2);             -- ERROR: 4
          V_AA_UP7  := NEW A_UP(A => 1, C => 3);             -- ERROR: 4
          V_AA_UP8  := NEW A_UP(1);                          -- ERROR: 4
          V_AA_UP9  := NEW A_UP(1, 2, 3, 4);                 -- ERROR: 4
          V_AA_UP10 := NEW A_UP(A => 1, B => 2, 3);          -- ERROR: 5
          V_AA_UP11 := NEW A_UP(A => 1, 2, C => 3);          -- ERROR: 5
          V_AA_UP12 := NEW A_UP(2, INT2, 7);                 -- ERROR: 6
          V_AA_UP13 := NEW A_UP(INT2, 4, 6);                 -- ERROR: 6
          V_AA_UP14 := NEW A_UP(OTHERS => 1);                -- ERROR: 7

          V_AA_UL0  := NEW A_UL(B => 1, C => 2, D => 3);     -- ERROR: 1
          V_AA_UL1  := NEW A_UL(A | B | A => 1);             -- ERROR: 2
          V_AA_UL2  := NEW A_UL(A => 1, B => 2, A => 3);     -- ERROR: 2
          V_AA_UL3  := NEW A_UL(A => 1, B => 2, A => 1);     -- ERROR: 2
          V_AA_UL4  := NEW A_UL(2, C => 3, A => 2);          -- ERROR: 3
          V_AA_UL5  := NEW A_UL(1, 2, A => 1);               -- ERROR: 3
          V_AA_UL6  := NEW A_UL(A => 1, B => 2);             -- ERROR: 4
          V_AA_UL7  := NEW A_UL(A => 1, C => 3);             -- ERROR: 4
          V_AA_UL8  := NEW A_UL(1);                          -- ERROR: 4
          V_AA_UL9  := NEW A_UL(1, 2, 3, 4);                 -- ERROR: 4
          V_AA_UL10 := NEW A_UL(A => 1, B => 2, 3);          -- ERROR: 5
          V_AA_UL11 := NEW A_UL(A => 1, 2, C => 3);          -- ERROR: 5
          V_AA_UL12 := NEW A_UL(2, INT2, 7);                 -- ERROR: 6
          V_AA_UL13 := NEW A_UL(INT2, 4, 6);                 -- ERROR: 6
          V_AA_UL14 := NEW A_UL(OTHERS => 1);                -- ERROR: 7

     END;

END B48002D;
