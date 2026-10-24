-- B34009E.ADA

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
-- CHECK THAT ONLY THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
-- (IMPLICITLY) FOR DERIVED NON-LIMITED PRIVATE TYPES WITH
-- DISCRIMINANTS.

-- JRK 10/2/86
-- PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;

PROCEDURE B34009E IS

     SUBTYPE COMPONENT IS INTEGER;

     PACKAGE PKG IS

          SUBTYPE LENGTH IS NATURAL RANGE 0 .. 10;

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 1) IS PRIVATE;

          FUNCTION CREATE ( B : BOOLEAN;
                            L : LENGTH;
                            I : INTEGER;
                            S : STRING;
                            C : COMPONENT;
                            F : FLOAT
                          ) RETURN PARENT;

     PRIVATE

          TYPE PARENT (B : BOOLEAN := TRUE; L : LENGTH := 1) IS
               RECORD
                    I : INTEGER := 2;
                    CASE B IS
                         WHEN TRUE =>
                              S : STRING (1 .. L) := (1 .. L => 'A');
                              C : COMPONENT := 2;
                         WHEN FALSE =>
                              F : FLOAT := 5.0;
                    END CASE;
               END RECORD;

     END PKG;

     USE PKG;

     TYPE T IS NEW PARENT;

     X, Y, Z : T;
     W       : PARENT;
     C       : COMPONENT := 1;

     N : CONSTANT := 1;
     R : CONSTANT := 1.0;

     B : BOOLEAN       := FALSE;
     I : INTEGER       := 0;
     F : FLOAT         := 0.0;
     D : DURATION      := 0.0;
     S : STRING (1..5) := "ABCDE";

     PROCEDURE P (X : T) IS
     BEGIN
          NULL;
     END P;

     PROCEDURE Q (X : PARENT) IS
     BEGIN
          NULL;
     END Q;

     PROCEDURE A (X : ADDRESS) IS
     BEGIN
          NULL;
     END A;

     PACKAGE BODY PKG IS

          FUNCTION CREATE
             ( B : BOOLEAN;
               L : LENGTH;
               I : INTEGER;
               S : STRING;
               C : COMPONENT;
               F : FLOAT
             ) RETURN PARENT
          IS
          BEGIN
               CASE B IS
                    WHEN TRUE =>
                         RETURN (TRUE, L, I, S, C);
                    WHEN FALSE =>
                         RETURN (FALSE, L, I, F);
               END CASE;
          END CREATE;

     END PKG;

BEGIN

     X := CREATE (TRUE, 3, 2, "AAA", 2, 1.0);
     Y := CREATE (TRUE, 3, 2, "AAA", 2, 1.0);
     Z := CREATE (TRUE, 3, 2, "AAA", 2, 1.0);
     W := CREATE (TRUE, 3, 2, "AAA", 2, 1.0);

     Z := X;                            -- OK:    :=.
     P (T'(X));                         -- OK:    QUALIFICATION.
     P (T (X));                         -- OK:    EXPLICIT CONVERSION.
     P (T (W));                         -- OK:    EXPLICIT CONVERSION.
     Q (PARENT (X));                    -- OK:    EXPLICIT CONVERSION.
     P (T (I));                         -- ERROR: EXPLICIT CONVERSION.
     I := INTEGER (X);                  -- ERROR: EXPLICIT CONVERSION.
     P (T (F));                         -- ERROR: EXPLICIT CONVERSION.
     F := FLOAT (X);                    -- ERROR: EXPLICIT CONVERSION.
     P (T (D));                         -- ERROR: EXPLICIT CONVERSION.
     D := DURATION (X);                 -- ERROR: EXPLICIT CONVERSION.
     P (T (S));                         -- ERROR: EXPLICIT CONVERSION.
     S := STRING (X);                   -- ERROR: EXPLICIT CONVERSION.
     P (N);                             -- ERROR: IMPLICIT CONVERSION.
     P (R);                             -- ERROR: IMPLICIT CONVERSION.
     P (3);                             -- ERROR: INTEGER LITERAL.
     P (3.0);                           -- ERROR: REAL LITERAL.
     P ('A');                           -- ERROR: CHARACTER LITERAL.
     P (TRUE);                          -- ERROR: ENUMERATION LITERAL.
     P ("AA");                          -- ERROR: STRING LITERAL.
     P (NULL);                          -- ERROR: NULL.
     P ((TRUE,2,1,"AB",C));             -- ERROR: AGGREGATE.
     P (NEW COMPONENT);                 -- ERROR: ALLOCATOR.
     I := X.L;                          -- OK:    SELECTION.
     C := X.C;                          -- ERROR: SELECTION.
     C := X.ALL;                        -- ERROR: .ALL.
     C := X (1);                        -- ERROR: INDEX.
     P (X (1..1));                      -- ERROR: SLICE.
     P (NOT X);                         -- ERROR: NOT.
     P (X AND Y);                       -- ERROR: AND.
     P (X OR Y);                        -- ERROR: OR.
     P (X XOR Y);                       -- ERROR: XOR.
     P (X AND THEN Y);                  -- ERROR: AND THEN.
     P (X OR ELSE Y);                   -- ERROR: OR ELSE.
     B := X = Y;                        -- OK:    =.
     B := X /= Y;                       -- OK:    /=.
     B := X < Y;                        -- ERROR: <.
     B := X > Y;                        -- ERROR: >.
     B := X <= Y;                       -- ERROR: <=.
     B := X >= Y;                       -- ERROR: >=.
     B := X IN T;                       -- OK:    IN.
     B := X NOT IN T;                   -- OK:    NOT IN.
     P (+X);                            -- ERROR: +.
     P (-X);                            -- ERROR: -.
     P (ABS X);                         -- ERROR: ABS.
     P (X + Y);                         -- ERROR: +.
     P (X - Y);                         -- ERROR: -.
     P (X * Y);                         -- ERROR: *.
     P (X * I);                         -- ERROR: *.
     P (I * X);                         -- ERROR: *.
     P (X / Y);                         -- ERROR: /.
     P (X / I);                         -- ERROR: /.
     P (X MOD Y);                       -- ERROR: MOD.
     P (X REM Y);                       -- ERROR: REM.
     P (X ** I);                        -- ERROR: **.
     P (X & Y);                         -- ERROR: &.
     P (X & C);                         -- ERROR: &.
     P (C & X);                         -- ERROR: &.
     P (C & C);                         -- ERROR: &.
     A (X'ADDRESS);                     -- OK:    'ADDRESS.
     I := T'AFT;                        -- ERROR: 'AFT.
     B := X'CALLABLE;                   -- ERROR: 'CALLABLE.
     B := T'CONSTRAINED;                -- OK:    'CONSTRAINED.
     B := X'CONSTRAINED;                -- OK:    'CONSTRAINED.
     I := X'COUNT;                      -- ERROR: 'COUNT.
     F := T'DELTA;                      -- ERROR: 'DELTA.
     I := T'DIGITS;                     -- ERROR: 'DIGITS.
     I := T'EMAX;                       -- ERROR: 'EMAX.
     F := T'EPSILON;                    -- ERROR: 'EPSILON.
     P (T'FIRST);                       -- ERROR: 'FIRST.
     I := X'FIRST;                      -- ERROR: 'FIRST.
     I := T'FIRST (1);                  -- ERROR: 'FIRST (N).
     I := X'FIRST (1);                  -- ERROR: 'FIRST (N).
     I := X'FIRST_BIT;                  -- ERROR: 'FIRST_BIT.
     I := T'FORE;                       -- ERROR: 'FORE.
     S := T'IMAGE (X);                  -- ERROR: 'IMAGE.
     F := T'LARGE;                      -- ERROR: 'LARGE.
     P (T'LAST);                        -- ERROR: 'LAST.
     I := X'LAST;                       -- ERROR: 'LAST.
     I := T'LAST (1);                   -- ERROR: 'LAST (N).
     I := X'LAST (1);                   -- ERROR: 'LAST (N).
     I := X'LAST_BIT;                   -- ERROR: 'LAST_BIT.
     I := T'LENGTH;                     -- ERROR: 'LENGTH.
     I := X'LENGTH;                     -- ERROR: 'LENGTH.
     I := T'LENGTH (1);                 -- ERROR: 'LENGTH (N).
     I := X'LENGTH (1);                 -- ERROR: 'LENGTH (N).
     I := T'MACHINE_EMAX;               -- ERROR: 'MACHINE_EMAX.
     I := T'MACHINE_EMIN;               -- ERROR: 'MACHINE_EMIN.
     I := T'MACHINE_MANTISSA;           -- ERROR: 'MACHINE_MANTISSA.
     B := T'MACHINE_OVERFLOWS;          -- ERROR: 'MACHINE_OVERFLOWS.
     I := T'MACHINE_RADIX;              -- ERROR: 'MACHINE_RADIX.
     B := T'MACHINE_ROUNDS;             -- ERROR: 'MACHINE_ROUNDS.
     I := T'MANTISSA;                   -- ERROR: 'MANTISSA.
     I := T'POS (X);                    -- ERROR: 'POS.
     I := X'POSITION;                   -- ERROR: 'POSITION.
     P (T'PRED (X));                    -- ERROR: 'PRED.
     FOR I IN T'RANGE LOOP              -- ERROR: 'RANGE.
          NULL;
     END LOOP;
     FOR I IN X'RANGE LOOP              -- ERROR: 'RANGE.
          NULL;
     END LOOP;
     FOR I IN T'RANGE (1) LOOP          -- ERROR: 'RANGE (N).
          NULL;
     END LOOP;
     FOR I IN X'RANGE (1) LOOP          -- ERROR: 'RANGE (N).
          NULL;
     END LOOP;
     I := T'SAFE_EMAX;                  -- ERROR: 'SAFE_EMAX.
     F := T'SAFE_LARGE;                 -- ERROR: 'SAFE_LARGE.
     F := T'SAFE_SMALL;                 -- ERROR: 'SAFE_SMALL.
     I := T'SIZE;                       -- OK:    'SIZE.
     I := X'SIZE;                       -- OK:    'SIZE.
     F := T'SMALL;                      -- ERROR: 'SMALL.
     I := T'STORAGE_SIZE;               -- ERROR: 'STORAGE_SIZE.
     I := X'STORAGE_SIZE;               -- ERROR: 'STORAGE_SIZE.
     P (T'SUCC (X));                    -- ERROR: 'SUCC.
     B := X'TERMINATED;                 -- ERROR: 'TERMINATED.
     P (T'VAL (1));                     -- ERROR: 'VAL.
     P (T'VALUE (S));                   -- ERROR: 'VALUE.
     I := T'WIDTH;                      -- ERROR: 'WIDTH.

END B34009E;
