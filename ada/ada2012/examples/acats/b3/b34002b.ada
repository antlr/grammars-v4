-- B34002B.ADA

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
-- (IMPLICITLY) FOR DERIVED INTEGER TYPES.

-- JRK 8/21/86
-- PWN 12/27/94  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;

PROCEDURE B34002B IS

     SUBTYPE COMPONENT IS INTEGER;

     TYPE PARENT IS RANGE -100 .. 100;

     TYPE T IS NEW PARENT;

     X, Y, Z : T         := 2;
     W       : PARENT    := 2;
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

BEGIN

     Z := X;                            -- OK:    :=.
     P (T'(X));                         -- OK:    QUALIFICATION.
     P (T (X));                         -- OK:    EXPLICIT CONVERSION.
     P (T (W));                         -- OK:    EXPLICIT CONVERSION.
     Q (PARENT (X));                    -- OK:    EXPLICIT CONVERSION.
     P (T (I));                         -- OK:    EXPLICIT CONVERSION.
     I := INTEGER (X);                  -- OK:    EXPLICIT CONVERSION.
     P (T (F));                         -- OK:    EXPLICIT CONVERSION.
     F := FLOAT (X);                    -- OK:    EXPLICIT CONVERSION.
     P (T (D));                         -- OK:    EXPLICIT CONVERSION.
     D := DURATION (X);                 -- OK:    EXPLICIT CONVERSION.
     P (T (S));                         -- ERROR: EXPLICIT CONVERSION.
     S := STRING (X);                   -- ERROR: EXPLICIT CONVERSION.
     P (N);                             -- OK:    IMPLICIT CONVERSION.
     P (R);                             -- ERROR: IMPLICIT CONVERSION.
     P (3);                             -- OK:    INTEGER LITERAL.
     P (3.0);                           -- ERROR: REAL LITERAL.
     P ('A');                           -- ERROR: CHARACTER LITERAL.
     P (TRUE);                          -- ERROR: ENUMERATION LITERAL.
     P ("AA");                          -- ERROR: STRING LITERAL.
     P (NULL);                          -- ERROR: NULL.
     P ((C,C));                         -- ERROR: AGGREGATE.
     P (NEW COMPONENT);                 -- ERROR: ALLOCATOR.
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
     B := X < Y;                        -- OK:    <.
     B := X > Y;                        -- OK:    >.
     B := X <= Y;                       -- OK:    <=.
     B := X >= Y;                       -- OK:    >=.
     B := X IN T;                       -- OK:    IN.
     B := X NOT IN T;                   -- OK:    NOT IN.
     P (+X);                            -- OK:    +.
     P (-X);                            -- OK:    -.
     P (ABS X);                         -- OK:    ABS.
     P (X + Y);                         -- OK:    +.
     P (X - Y);                         -- OK:    -.
     P (X * Y);                         -- OK:    *.
     P (X * I);                         -- ERROR: *.
     P (I * X);                         -- ERROR: *.
     P (X / Y);                         -- OK:    /.
     P (X / I);                         -- ERROR: /.
     P (X MOD Y);                       -- OK:    MOD.
     P (X REM Y);                       -- OK:    REM.
     P (X ** I);                        -- OK:    **.
     P (X & Y);                         -- ERROR: &.
     P (X & C);                         -- ERROR: &.
     P (C & X);                         -- ERROR: &.
     P (C & C);                         -- ERROR: &.
     A (X'ADDRESS);                     -- OK:    'ADDRESS.
     I := T'AFT;                        -- ERROR: 'AFT.
     I := T'BASE'SIZE;                  -- OK:    'BASE.
     B := X'CALLABLE;                   -- ERROR: 'CALLABLE.
     B := T'CONSTRAINED;                -- ERROR: 'CONSTRAINED.
     B := X'CONSTRAINED;                -- ERROR: 'CONSTRAINED.
     I := X'COUNT;                      -- ERROR: 'COUNT.
     F := T'DELTA;                      -- ERROR: 'DELTA.
     I := T'DIGITS;                     -- ERROR: 'DIGITS.
     I := T'EMAX;                       -- ERROR: 'EMAX.
     F := T'EPSILON;                    -- ERROR: 'EPSILON.
     P (T'FIRST);                       -- OK:    'FIRST.
     I := X'FIRST;                      -- ERROR: 'FIRST.
     I := T'FIRST (1);                  -- ERROR: 'FIRST (N).
     I := X'FIRST (1);                  -- ERROR: 'FIRST (N).
     I := X'FIRST_BIT;                  -- ERROR: 'FIRST_BIT.
     I := T'FORE;                       -- ERROR: 'FORE.
     S := T'IMAGE (X);                  -- OK:    'IMAGE.
     F := T'LARGE;                      -- ERROR: 'LARGE.
     P (T'LAST);                        -- OK:    'LAST.
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
     I := T'POS (X);                    -- OK:    'POS.
     I := X'POSITION;                   -- ERROR: 'POSITION.
     P (T'PRED (X));                    -- OK:    'PRED.
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
     P (T'SUCC (X));                    -- OK:    'SUCC.
     B := X'TERMINATED;                 -- ERROR: 'TERMINATED.
     P (T'VAL (1));                     -- OK:    'VAL.
     P (T'VALUE (S));                   -- OK:    'VALUE.
     I := T'WIDTH;                      -- OK:    'WIDTH.

END B34002B;
