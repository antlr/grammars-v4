-- B43101A.ADA

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
-- CHECK THAT EACH OF THE FOLLOWING IS ILLEGAL:

--   A) TWO CHOICES WITH THE SAME IDENTIFIER.

--   B) A CHOICE NAMING A COMPONENT WHOSE VALUE WAS GIVEN PREVIOUSLY
--      BY A POSITIONAL COMPONENT ASSOCIATION.

--   C) A CHOICE THAT IS NOT THE IDENTIFIER OF A COMPONENT.

--   D) A COMPONENT ASSOCIATION WITH MORE THAN ONE CHOICE OR WITH THE
--      SINGLE CHOICE OTHERS, WHERE THE CORRESPONDING COMPONENTS ARE
--      OF DIFFERENT TYPES.

--   E) A COMPONENT ASSOCIATION WITH THE CHOICE OTHERS, WHERE THE OTHERS
--      CHOICE DOES NOT REPRESENT AT LEAST ONE COMPONENT OF THE RECORD.

--   F) A VALUE IS NOT PROVIDED FOR EVERY COMPONENT OF THE RECORD
--      SUBTYPE.

--   G) A RANGE USING COMPONENT NAMES FOR LOWER AND UPPER BOUNDS.

-- TYPE OF ERRORS:
--   1 - ERROR WHILE AGGREGATE IS NOT OVERLOADED.
--   2 - ERROR WHILE AGGREGATE IS A LEGAL AGGREGATE OF SOME RECORD
--       TYPE, BUT FAILS THE CHECK FOR SOME OTHER RECORD TYPE, AND
--       ITS TYPE IS NOT UNIQUELY DETERMINED BY THE CONTEXT.
--   3 - ERROR WHILE AGGREGATE IS A LEGAL AGGREGATE OF SOME ARRAY
--       TYPE, BUT FAILS THE CHECK FOR SOME RECORD TYPE, AND
--       ITS TYPE IS NOT UNIQUELY DETERMINED BY THE CONTEXT.

-- EG  02/14/84

PROCEDURE B43101A IS

BEGIN

--   A) TWO CHOICES WITH THE SAME IDENTIFIER.

     DECLARE

          TYPE R1 IS
               RECORD
                    A : INTEGER;
                    B : INTEGER;
                    C : INTEGER;
               END RECORD;

          A1 : R1 := (A => 1, C => 3, A => 4, B => 2); -- ERROR: A1.
          A2 : CONSTANT R1 := (A => 1, B => 2, C => 3,
                               B => 2);                -- ERROR: A1.
          A3 : R1;

     BEGIN

          A3 := (C => 3, A => 1, B => 2, A => 1);      -- ERROR: A1.

     END;

--   B) A CHOICE NAMING A COMPONENT WHOSE VALUE WAS GIVEN PREVIOUSLY
--      BY A POSITIONAL COMPONENT ASSOCIATION.

     DECLARE

          TYPE R1 IS
               RECORD
                    A : INTEGER;
                    B : INTEGER;
                    C : INTEGER;
                    D : INTEGER;
               END RECORD;

          B1 : R1 := (-2, -3, D => -4, A => -1);       -- ERROR: B1.
          B2 : CONSTANT R1 := (1, 2, 3, B => 2);       -- ERROR: B1.
          B3 : R1;

     BEGIN

          B3 := (4, C => 2, D => 1, A => 1);           -- ERROR: B1.

     END;

--   C) A CHOICE THAT IS NOT THE IDENTIFIER OF A COMPONENT.

     DECLARE

          SUBTYPE ST1 IS INTEGER RANGE 1 .. 2;
          TYPE R1 IS
               RECORD
                    A : INTEGER;
                    B : INTEGER;
                    C : INTEGER;
               END RECORD;
          TYPE R2 (A : ST1) IS
               RECORD
                    CASE A IS
                         WHEN 1 =>
                              B : INTEGER;
                         WHEN 2 =>
                              C : INTEGER;
                    END CASE;
               END RECORD;

          C1 : R1 := (1, 2, 3, D => 4);                -- ERROR: C1.
          C2 : CONSTANT R1 := (1, 2, D => 3, C => 4);  -- ERROR: C1.
          C3 : R1;
          C4 : R2(1) := (1, C => 2);                   -- ERROR: C1.
          C5 : CONSTANT R2(2) := (2, B => -1);         -- ERROR: C1.
          C6 : R2(1);

     BEGIN

          C3 := (A => 1, D => 4, C => 3, B => 2);      -- ERROR: C1.
          C6 := (1, C => -1);                          -- ERROR: C1.

     END;

--   D) A COMPONENT ASSOCIATION WITH MORE THAN ONE CHOICE OR WITH THE
--      SINGLE CHOICE OTHERS, WHERE THE CORRESPONDING COMPONENTS ARE
--      OF DIFFERENT TYPES.

     DECLARE

          TYPE MY_INTEGER IS RANGE -5 .. 5;
          TYPE R1 IS
               RECORD
                    A : INTEGER;
                    B : MY_INTEGER;
                    C : INTEGER;
               END RECORD;

          D1 : R1 := (A => -1, OTHERS => -2);          -- ERROR: D1.
          D2 : CONSTANT R1 := (A | B => -2, C => -3);  -- ERROR: D1.
          D3 : R1;

     BEGIN

          D3 := (C => 1, OTHERS => 4);                 -- ERROR: D1.

     END;

--   E) A COMPONENT ASSOCIATION WITH THE CHOICE OTHERS, WHERE THE OTHERS
--      CHOICE DOES NOT REPRESENT AT LEAST ONE COMPONENT OF THE RECORD.

     DECLARE

          TYPE T1 IS ARRAY(1 .. 4) OF INTEGER;
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

          E1 : R1 := (1, 2, 3, OTHERS => 4);           -- ERROR: E1.
          E2 : CONSTANT R1 := (B => 2, A => 1, C => 3,
                               OTHERS => 4);           -- ERROR: E1.
          E3 : R1;

          PROCEDURE PROC1 (A : R1) IS
          BEGIN
               NULL;
          END PROC1;
          PROCEDURE PROC1 (A : R2) IS
          BEGIN
               NULL;
          END PROC1;

          PROCEDURE PROC2 (A : T1) IS
          BEGIN
               NULL;
          END PROC2;
          PROCEDURE PROC2 (A : R2) IS
          BEGIN
               NULL;
          END PROC2;

     BEGIN

          E3 := (1, 2, C => 3, OTHERS => 4);           -- ERROR: E1.
          PROC1((A => -1, OTHERS => -2));              -- ERROR: E2.
          PROC1((A | B => -2, C => -3, OTHERS => -4)); -- ERROR: E2.
          PROC2((1, 2, 3, OTHERS => 4));               -- ERROR: E3.

     END;

--   F) A VALUE IS NOT PROVIDED FOR EVERY COMPONENT OF THE RECORD
--      SUBTYPE.

     DECLARE

          TYPE ENUM IS (B, C);
          TYPE T1 IS ARRAY(ENUM RANGE B .. C) OF INTEGER;
          SUBTYPE ST1 IS INTEGER RANGE 1 .. 2;
          TYPE R1 IS
               RECORD
                    A : INTEGER;
                    B : INTEGER;
                    C : INTEGER;
               END RECORD;
          TYPE R2 (A : ST1) IS
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
          TYPE R3 IS
               RECORD
                    B : INTEGER;
                    C : INTEGER;
               END RECORD;

          F1 : R1 := (1, 2);                           -- ERROR: F1.
          F2 : CONSTANT R1 := (B => 2, C => 3);        -- ERROR: F1.
          F3 : R1;
          F4 : R2(1) := (1, 5, 4);                     -- ERROR: F1.
          F5 : CONSTANT R2(2) := (2, 3);               -- ERROR: F1.
          F6 : R2(2);

          PROCEDURE PROC1 (A : R1) IS
          BEGIN
               NULL;
          END PROC1;
          PROCEDURE PROC1 (A : R3) IS
          BEGIN
               NULL;
          END PROC1;

          PROCEDURE PROC2 (A : T1) IS
          BEGIN
               NULL;
          END PROC2;
          PROCEDURE PROC2 (A : R1) IS
          BEGIN
               NULL;
          END PROC2;

     BEGIN

          F3 := (1, C => 3);                           -- ERROR: F1.
          F6 := (2, 1, 2, 3);                          -- ERROR: F1.
          PROC1 ((B => 2, C => 3));                    -- ERROR: F2.
          PROC2 ((B => 2, C => 3));                    -- ERROR: F3.

     END;

--   G) A RANGE USING COMPONENT NAMES FOR LOWER AND UPPER BOUNDS.

     DECLARE

          TYPE ENUM IS (A, B, C, D);
          TYPE T1 IS ARRAY(ENUM RANGE A .. D) OF INTEGER;
          TYPE R1 IS
               RECORD
                    A : INTEGER;
                    B : INTEGER;
                    C : INTEGER;
                    D : INTEGER;
               END RECORD;

          G1 : R1 := (A .. C | D => 1);                -- ERROR: G1.
          G2 : CONSTANT R1 := (A .. D => -1);          -- ERROR: G1.
          G3 : R1;

          PROCEDURE PROC1 (A : T1) IS
          BEGIN
               NULL;
          END PROC1;
          PROCEDURE PROC1 (A : R1) IS
          BEGIN
               NULL;
          END PROC1;

     BEGIN

          G3 := (A .. B => -1, C .. D => -2);          -- ERROR: G1.
          PROC1 ((A .. C | D => 1));                   -- ERROR: G3.

     END;

END B43101A;
