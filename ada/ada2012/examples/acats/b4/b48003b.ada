-- B48003B.ADA

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
-- FOR ALLOCATORS OF THE FORM "NEW T'(X)", CHECK THAT IF T IS A RECORD
-- TYPE AND (X) IS AN AGGREGATE, CHECK ALL ILLEGAL FORMS OF RECORD
-- AGGREGATE.

-- EG  08/02/84
-- JBG 10/21/85  B2 INITIAL VALUE SET TO BE AN ALLOCATOR (FRN 85-10-15).
-- RLB 11/19/19  Added error location indicators.

PROCEDURE B48003B IS

     TYPE R1 IS
          RECORD
               A : INTEGER;
               B : INTEGER;
               C : INTEGER;
          END RECORD;
     TYPE R2 IS
          RECORD
               A : INTEGER;
               B : INTEGER;
               C : INTEGER;
               D : INTEGER;
          END RECORD;

     TYPE A_R1 IS ACCESS R1;
     TYPE A_R2 IS ACCESS R2;

BEGIN

--   A) TWO CHOICES WITH THE SAME IDENTIFIER.

     DECLARE

          A1 : A_R1 := NEW R1'(A => 1, C => 3,
                               A => 4, B => 2);        -- ERROR: A1. {1:24}
          A2 : CONSTANT A_R1 := NEW R1'(A => 1, B => 2, C => 3,
                                        B => 2);       -- ERROR: A1. {1:33}
          A3 : A_R1;

     BEGIN

          A3 := NEW R1'(C => 3, A => 1, B => 2,
                        A => 1);                       -- ERROR: A1. {1:17}

     END;

--   B) A CHOICE NAMING A COMPONENT WHOSE VALUE WAS GIVEN PREVIOUSLY
--      BY A POSITIONAL COMPONENT ASSOCIATION.

     DECLARE

          B1 : A_R2 := NEW R2'(-2, -3, D => -4,
                               A => -1);               -- ERROR: B1. {1:24}
          B2 : CONSTANT A_R2 := NEW R2'(1, 2, 3, B => 2);  -- ERROR: B1. {33}
          B3 : A_R2;

     BEGIN

          B3 := NEW R2'(4, C => 2, D => 1, A => 1);    -- ERROR: B1. {17}

     END;

--   C) A CHOICE THAT IS NOT THE IDENTIFIER OF A COMPONENT.

     DECLARE

          SUBTYPE ST1 IS INTEGER RANGE 1 .. 2;
          TYPE R3 (A : ST1) IS
               RECORD
                    CASE A IS
                         WHEN 1 =>
                              B : INTEGER;
                         WHEN 2 =>
                              C : INTEGER;
                    END CASE;
               END RECORD;
          TYPE A_R3 IS ACCESS R3;

          C1 : A_R1 := NEW R1'(1, 2, 3, D => 4);       -- ERROR: C1. {24}
          C2 : CONSTANT A_R1 := NEW R1'(1, 2, D => 3,
                                        C => 4);       -- ERROR: C1. {1:33}
          C3 : A_R1;
          C4 : A_R3 := NEW R3'(1, C => 2);             -- ERROR: C1. {24}
          C5 : CONSTANT A_R3 := NEW R3'(2, B => -1);   -- ERROR: C1. {33}
          C6 : A_R3;

     BEGIN

          C3 := NEW R1'(A => 1, D => 4,
                        C => 3, B => 2);               -- ERROR: C1. {1:17}
          C6 := NEW R3'(1, C => -1);                   -- ERROR: C1. {17}

     END;

--   D) A COMPONENT ASSOCIATION WITH MORE THAN ONE CHOICE OR WITH THE
--      SINGLE CHOICE OTHERS, WHERE THE CORRESPONDING COMPONENTS ARE
--      OF DIFFERENT TYPES.

     DECLARE

          TYPE MY_INTEGER IS RANGE -5 .. 5;
          TYPE R4 IS
               RECORD
                    A : INTEGER;
                    B : MY_INTEGER;
                    C : INTEGER;
               END RECORD;
          TYPE A_R4 IS ACCESS R4;

          D1 : A_R4 := NEW R4'(A => -1, OTHERS => -2); -- ERROR: D1. {24}
          D2 : CONSTANT A_R4 := NEW R4'(A | B => -2,
                                        C => -3);      -- ERROR: D1. {1:33}
          D3 : A_R4;

     BEGIN

          D3 := NEW R4'(C => 1, OTHERS => 4);          -- ERROR: D1. {17}

     END;

--   E) A COMPONENT ASSOCIATION WITH THE CHOICE OTHERS, WHERE THE OTHERS
--      CHOICE DOES NOT REPRESENT AT LEAST ONE COMPONENT OF THE RECORD.

     DECLARE

          E1 : A_R1 := NEW R1'(1, 2, 3, OTHERS => 4);  -- ERROR: E1. {24}
          E2 : CONSTANT A_R1 := NEW R1'(B => 2, A => 1, C => 3,
                               OTHERS => 4);           -- ERROR: E1. {1:33}
          E3 : A_R1;

     BEGIN

          E3 := NEW R1'(1, 2, C => 3, OTHERS => 4);    -- ERROR: E1. {17}

     END;

--   F) A VALUE IS NOT PROVIDED FOR EVERY COMPONENT OF THE RECORD
--      SUBTYPE.

     DECLARE

          SUBTYPE ST1 IS INTEGER RANGE 1 .. 2;
          TYPE R5 (A : ST1) IS
               RECORD
                    CASE A IS
                         WHEN 1 =>
                              B : INTEGER;
                              C : INTEGER;
                              D : INTEGER;
                         WHEN 2 =>
                              E : INTEGER;
                              F : INTEGER;
                    END CASE;
               END RECORD;
          TYPE A_R5 IS ACCESS R5;

          F1 : A_R1 := NEW R1'(1, 2);                  -- ERROR: F1. {24}
          F2 : CONSTANT A_R1 := NEW R1'(B => 2, C => 3);   -- ERROR: F1. {33}
          F3 : A_R1;
          F4 : A_R5(1) := NEW R5'(1, 5, 4);            -- ERROR: F1. {27}
          F5 : CONSTANT A_R5(2) := NEW R5'(2, 3);      -- ERROR: F1. {36}
          F6 : A_R5(2);

     BEGIN

          F3 := NEW R1'(1, C => 3);                    -- ERROR: F1. {17}
          F6 := NEW R5'(2, 1, 2, 3);                   -- ERROR: F1. {17}

     END;

--   G) A RANGE USING COMPONENT NAMES FOR LOWER AND UPPER BOUNDS.

     DECLARE

          G1 : A_R2 := NEW R2'(A .. C | D => 1);       -- ERROR: G1. {24}
          G2 : CONSTANT A_R2 := NEW R2'(A .. D => -1); -- ERROR: G1. {33}
          G3 : A_R2;

     BEGIN

          G3 := NEW R2'(A .. B => -1, C .. D => -2);   -- ERROR: G1. {17}

     END;

END B48003B;
