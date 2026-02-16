-- B34005N.ADA

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
-- OBJECTIVE:
--     CHECK THAT ONLY THE REQUIRED PREDEFINED OPERATIONS ARE DECLARED
--     (IMPLICITLY) FOR DERIVED MULTI-DIMENSIONAL ARRAY TYPES WHOSE
--     COMPONENT TYPE IS A NON-LIMITED TYPE.

-- HISTORY:
--     JRK 9/17/86  CREATED ORIGINAL TEST.
--     PWN 02/02/95  REMOVED INCONSISTENCIES WITH ADA 9X.
--     PWN 10/27/95  REMOVED CHECK THAT REQUIRES UNIVERSAL_INTEGER
--                   FOR AN ATTRIBUTE.
--     PWN 02/01/96  RESTORED CHECKS IN ADA 95 LEGAL FORMAT.
--                   3.6.2(1);6.0

WITH SYSTEM; USE SYSTEM;

PROCEDURE B34005N IS

     SUBTYPE COMPONENT IS INTEGER;

     TYPE PARENT IS ARRAY (1 .. 2, 1 .. 3) OF COMPONENT;

     TYPE T IS NEW PARENT;

     TYPE ARR IS ARRAY (0 .. 1, 2 .. 4) OF COMPONENT;

     X, Y, Z : T         := (OTHERS => (OTHERS => 2));
     W       : PARENT    := (OTHERS => (OTHERS => 2));
     C       : COMPONENT := 1;

     N : CONSTANT := 1;
     R : CONSTANT := 1.0;

     B : BOOLEAN       := FALSE;
     I : INTEGER       := 0;
     F : FLOAT         := 0.0;
     D : DURATION      := 0.0;
     S : STRING (1..5) := "ABCDE";
     U : ARR           := (OTHERS => (OTHERS => C));

     J :          INTEGER := 1;
     K : CONSTANT INTEGER := 1;

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

     FUNCTION V RETURN T IS
     BEGIN
          RETURN (OTHERS => (OTHERS => C));
     END V;

BEGIN

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
     P (T (U));                         -- OK:    EXPLICIT CONVERSION.
     U := ARR (X);                      -- OK:    EXPLICIT CONVERSION.
     P (N);                             -- ERROR: IMPLICIT CONVERSION.
     P (R);                             -- ERROR: IMPLICIT CONVERSION.
     P (3);                             -- ERROR: INTEGER LITERAL.
     P (3.0);                           -- ERROR: REAL LITERAL.
     P ('A');                           -- ERROR: CHARACTER LITERAL.
     P (TRUE);                          -- ERROR: ENUMERATION LITERAL.
     P ("AA");                          -- ERROR: STRING LITERAL.
     P (NULL);                          -- ERROR: NULL.
     P (((C,C,C),(C,C,C)));             -- OK:    AGGREGATE.
     P (NEW COMPONENT);                 -- ERROR: ALLOCATOR.
     C := X.C;                          -- ERROR: SELECTION.
     C := X.ALL;                        -- ERROR: .ALL.
     C := X (1,1);                      -- OK:    INDEX.
     Z (1,1) := C;                      -- OK:    INDEX.
     P (X (1..2));                      -- ERROR: SLICE.
     Z (1..2) := X;                     -- ERROR: SLICE.
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
     B := T'CONSTRAINED;                -- ERROR: 'CONSTRAINED.
     B := X'CONSTRAINED;                -- ERROR: 'CONSTRAINED.
     I := X'COUNT;                      -- ERROR: 'COUNT.
     F := T'DELTA;                      -- ERROR: 'DELTA.
     I := T'DIGITS;                     -- ERROR: 'DIGITS.
     I := T'EMAX;                       -- ERROR: 'EMAX.
     F := T'EPSILON;                    -- ERROR: 'EPSILON.
     I := T'FIRST;                      -- OK:    'FIRST.
     I := X'FIRST;                      -- OK:    'FIRST.
     I := V'FIRST;                      -- OK:    'FIRST.
     I := T'FIRST (2);                  -- OK:    'FIRST (N).
     I := X'FIRST (2);                  -- OK:    'FIRST (N).
     I := V'FIRST (2);                  -- OK:    'FIRST (N).
     I := T'FIRST (0);                  -- ERROR: DIMENSION 0.
     I := X'FIRST_BIT;                  -- ERROR: 'FIRST_BIT.
     I := T'FORE;                       -- ERROR: 'FORE.
     S := T'IMAGE (X);                  -- ERROR: 'IMAGE.
     F := T'LARGE;                      -- ERROR: 'LARGE.
     I := T'LAST;                       -- OK:    'LAST.
     I := X'LAST;                       -- OK:    'LAST.
     I := V'LAST;                       -- OK:    'LAST.
     I := T'LAST (2);                   -- OK:    'LAST (N).
     I := X'LAST (2);                   -- OK:    'LAST (N).
     I := V'LAST (2);                   -- OK:    'LAST (N).
     I := T'LAST (3);                   -- ERROR: DIMENSION 3.
     I := X'LAST_BIT;                   -- ERROR: 'LAST_BIT.
     I := T'LENGTH;                     -- OK:    'LENGTH.
     I := X'LENGTH;                     -- OK:    'LENGTH.
     I := V'LENGTH;                     -- OK:    'LENGTH.
     I := T'LENGTH (2);                 -- OK:    'LENGTH (N).
     I := X'LENGTH (2);                 -- OK:    'LENGTH (N).
     I := V'LENGTH (2);                 -- OK:    'LENGTH (N).
     I := T'LENGTH (K);                 -- OK:    NOT UNIVERSAL_INTEGER.
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
     FOR I IN T'RANGE LOOP              -- OK:    'RANGE.
          NULL;
     END LOOP;
     FOR I IN X'RANGE LOOP              -- OK:    'RANGE.
          NULL;
     END LOOP;
     FOR I IN V'RANGE LOOP              -- OK:    'RANGE.
          NULL;
     END LOOP;
     FOR I IN T'RANGE (2) LOOP          -- OK:    'RANGE (N).
          NULL;
     END LOOP;
     FOR I IN X'RANGE (2) LOOP          -- OK:    'RANGE (N).
          NULL;
     END LOOP;
     FOR I IN V'RANGE (2) LOOP          -- OK:    'RANGE (N).
          NULL;
     END LOOP;
     FOR I IN T'RANGE (INTEGER'POS (J)) LOOP  -- ERROR: NOT STATIC.
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

END B34005N;
