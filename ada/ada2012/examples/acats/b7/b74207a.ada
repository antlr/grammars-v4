-- B74207A.ADA

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
-- CHECK THAT NO ATTRIBUTES EXCEPT 'BASE, 'SIZE, AND 'CONSTRAINED CAN BE
-- APPLIED TO A PRIVATE TYPE OUTSIDE ITS PACKAGE.  IN PARTICULAR, CHECK
-- THAT ATTRIBUTES LEGAL FOR THE FULL DECLARATION OF THE PRIVATE TYPE
-- ARE NOT ALLOWED:

--     FOR FIXED POINT: 'AFT, 'DELTA, 'FIRST, 'FORE, 'LARGE, 'LAST,
--        'MACHINE_OVERFLOWS, 'MACHINE_ROUNDS, 'MANTISSA, 'SAFE_LARGE,
--        'SAFE_SMALL, 'SMALL.

--     FOR FLOATING POINT: 'DIGITS, 'EMAX, 'EPSILON, 'FIRST, 'LARGE,
--        'LAST, 'MACHINE_EMAX, 'MACHINE_EMIN, 'MACHINE_MANTISSA,
--        'MACHINE_OVERFLOWS, 'MACHINE_RADIX, 'MACHINE_ROUNDS,
--        'MANTISSA, 'SAFE_EMAX, 'SAFE_LARGE, 'SAFE_SMALL, 'SMALL.

--     FOR INTEGER: 'FIRST, 'IMAGE, 'LAST, 'POS, 'PRED, 'SUCC, 'VAL,
--        'VALUE, 'WIDTH.

--     FOR ARRAY: 'FIRST, 'LAST, 'LENGTH, 'RANGE.

--     FOR ACCESS: 'STORAGE_SIZE.


-- DSJ 4/29/83
-- JBG 8/21/83
-- JBG 9/22/83
-- JRK 1/6/84
-- JRK 8/24/84
-- JBG 11/8/85 AVOID CONFLICT WITH AI-7 AND AI-275
-- DTN 11/19/91 DELETED SUBPART (A).
-- KAS 11/16/95 REMOVE ADA 83 ATTRIBUTE REFERENCES
-- TMB 11/19/96 REMOVED 'STORAGE_SIZE CHECK FOR TASK TYPE
-- TMB 12/1/96  REMOVED 'SMALL CHECK FOR FLOATING POINT TYPE

PROCEDURE B74207A IS


     PACKAGE Q IS
          TYPE FIX IS PRIVATE;
          TYPE FLT IS LIMITED PRIVATE;
          TYPE INT IS PRIVATE;
          TYPE ARR IS LIMITED PRIVATE;
          TYPE REC IS PRIVATE;
          TYPE RECD (D : INTEGER) IS LIMITED PRIVATE;
          TYPE ACC IS PRIVATE;
          TYPE TSK IS LIMITED PRIVATE;
     PRIVATE
          TYPE FIX IS DELTA 0.5 RANGE -10.0 .. 10.0;
          TYPE FLT IS DIGITS 1;
          TYPE INT IS RANGE -10 .. 10;
          TYPE ARR IS ARRAY (1..2) OF INTEGER;
          TYPE REC IS
               RECORD NULL; END RECORD;
          TYPE RECD (D : INTEGER) IS
               RECORD NULL; END RECORD;
          TYPE ACC IS ACCESS ARR;
          TASK TYPE TSK;

          FIX0 : INTEGER := FIX'SIZE;        -- OK.
          FIX2 : INTEGER := FIX'BASE'SIZE;   -- OK.
          FIXA : INTEGER  := FIX'AFT;        -- OK: 'AFT.
          FIXB : FLOAT    := FIX'DELTA;      -- OK: 'DELTA.
          FIXC : FIX      := FIX'FIRST;      -- OK: 'FIRST.
          FIXD : INTEGER  := FIX'FORE;       -- OK: 'FORE.
          FIXF : FIX      := FIX'LAST;       -- OK: 'LAST.
          FIXG : BOOLEAN := FIX'MACHINE_OVERFLOWS; -- OK: ATTR.
          FIXH : BOOLEAN := FIX'MACHINE_ROUNDS;    -- OK: ATTR.
          FIXL : FLOAT    := FIX'SMALL;      -- OK: 'SMALL.

          FLT0 : INTEGER := FLT'SIZE;        -- OK.
          FLT2 : INTEGER := FLT'BASE'SIZE;   -- OK.
          FLTA : INTEGER  := FLT'DIGITS;     -- OK: 'DIGITS.
          FLTD : FLT      := FLT'FIRST;      -- OK: 'FIRST.
          FLTF : FLT      := FLT'LAST;       -- OK: 'LAST.
          FLTG : INTEGER  := FLT'MACHINE_EMAX;    -- OK: ATTR.
          FLTH : INTEGER  := FLT'MACHINE_EMIN;    -- OK: ATTR.
          FLTI : INTEGER  := FLT'MACHINE_MANTISSA;-- OK: ATTR.
          FLTJ : BOOLEAN  := FLT'MACHINE_OVERFLOWS;-- OK: ATTR.
          FLTK : INTEGER  := FLT'MACHINE_RADIX;   -- OK: ATTR.
          FLTL : BOOLEAN  := FLT'MACHINE_ROUNDS;  -- OK: ATTR.

          INT0 : INTEGER := INT'SIZE;        -- OK.
          INT2 : INTEGER := INT'BASE'SIZE;   -- OK.
          INTA : INT     := INT'FIRST;       -- OK: 'FIRST.
          X : INT;
          INTB : STRING (1..3) := INT'IMAGE(X);   -- OK: 'IMAGE.
          INTC : INT     := INT'LAST;        -- OK: 'LAST.
          INTD : INT     := INT'POS(X);      -- OK: 'POS.
          INTE : INT     := INT'PRED(X);     -- OK: 'PRED.
          INTF : INT     := INT'SUCC(X);     -- OK: 'SUCC.
          INTG : INT     := INT'VAL(1);      -- OK: 'VAL.
          INTH : INT     := INT'VALUE("1");  -- OK: 'VALUE.
          INTI : INTEGER := INT'WIDTH;       -- OK: 'WIDTH.

          ARR0 : INTEGER := ARR'SIZE;        -- OK.
          ARRA : INTEGER := ARR'FIRST;       -- OK: 'FIRST.
          ARRB : INTEGER := ARR'LAST;        -- OK: 'LAST.
          ARRC : INTEGER := ARR'LENGTH;      -- OK: 'LENGTH.
          ARRD : BOOLEAN := 3 IN ARR'RANGE;  -- OK: 'RANGE.

          REC0 : INTEGER := REC'SIZE;        -- OK.

          RECD0 : INTEGER := RECD'SIZE;        -- OK.

          ACC0 : INTEGER := ACC'SIZE;        -- OK.
          ACCA : INTEGER := ACC'STORAGE_SIZE;-- OK: ATTR.

          TSK0 : INTEGER := TSK'SIZE;        -- OK.

     END Q;

     PACKAGE BODY Q IS
          TASK BODY TSK IS
          BEGIN
               NULL;
          END TSK;
     END Q;
     USE Q;

     PROCEDURE FLTFLT (X : FLT) IS
     BEGIN
          NULL;
     END FLTFLT;

     PACKAGE TEST_ATTRIBUTES IS
          FIX0 : INTEGER := FIX'SIZE;        -- OK.
          FIX1 : BOOLEAN := FIX'CONSTRAINED; -- OK.
          FIXA : INTEGER  := FIX'AFT;        -- ERROR: 'AFT.
          FIXB : FLOAT    := FIX'DELTA;      -- ERROR: 'DELTA.
          FIXC : FIX      := FIX'FIRST;      -- ERROR: 'FIRST.
          FIXD : INTEGER  := FIX'FORE;       -- ERROR: 'FORE.
          FIXF : FIX      := FIX'LAST;       -- ERROR: 'LAST.
          FIXG : BOOLEAN := FIX'MACHINE_OVERFLOWS; -- ERROR: ATTR.
          FIXH : BOOLEAN := FIX'MACHINE_ROUNDS;    -- ERROR: ATTR.
          FIXL : FLOAT    := FIX'SMALL;      -- ERROR: 'SMALL.

          FLT0 : INTEGER := FLT'SIZE;        -- OK.
          FLT1 : BOOLEAN := FLT'CONSTRAINED; -- OK.
          FLTA : INTEGER  := FLT'DIGITS;     -- ERROR: 'DIGITS.
          FLTG : INTEGER  := FLT'MACHINE_EMAX;    -- ERROR: ATTR.
          FLTH : INTEGER  := FLT'MACHINE_EMIN;    -- ERROR: ATTR.
          FLTI : INTEGER  := FLT'MACHINE_MANTISSA;-- ERROR: ATTR.
          FLTJ : BOOLEAN  := FLT'MACHINE_OVERFLOWS;-- ERROR: ATTR.
          FLTK : INTEGER  := FLT'MACHINE_RADIX;   -- ERROR: ATTR.
          FLTL : BOOLEAN  := FLT'MACHINE_ROUNDS;  -- ERROR: ATTR.
          FLTQ : FLOAT    := FLT'SMALL;      -- ERROR: 'SMALL.

          INT0 : INTEGER := INT'SIZE;        -- OK.
          INT1 : BOOLEAN := INT'CONSTRAINED; -- OK.
          INTA : INT     := INT'FIRST;       -- ERROR: 'FIRST.
          X : INT;
          INTB : STRING (1..3) := INT'IMAGE(X);   -- ERROR: 'IMAGE.
          INTC : INT     := INT'LAST;        -- ERROR: 'LAST.
          INTD : INT     := INT'POS(X);      -- ERROR: 'POS.
          INTE : INT     := INT'PRED(X);     -- ERROR: 'PRED.
          INTF : INT     := INT'SUCC(X);     -- ERROR: 'SUCC.
          INTG : INT     := INT'VAL(1);      -- ERROR: 'VAL.
          INTH : INT     := INT'VALUE("1");  -- ERROR: 'VALUE.
          INTI : INTEGER := INT'WIDTH;       -- ERROR: 'WIDTH.

          ARR0 : INTEGER := ARR'SIZE;        -- OK.
          ARR1 : BOOLEAN := ARR'CONSTRAINED; -- OK.
          ARRA : INTEGER := ARR'FIRST;       -- ERROR: 'FIRST.
          ARRB : INTEGER := ARR'LAST;        -- ERROR: 'LAST.
          ARRC : INTEGER := ARR'LENGTH;      -- ERROR: 'LENGTH.
          ARRD : BOOLEAN := 3 IN ARR'RANGE;  -- ERROR: 'RANGE.

          REC0 : INTEGER := REC'SIZE;        -- OK.
          REC1 : BOOLEAN := REC'CONSTRAINED; -- OK.

          RECD0 : INTEGER := RECD'SIZE;        -- OK.
          RECD1 : BOOLEAN := RECD'CONSTRAINED; -- OK.

          ACC0 : INTEGER := ACC'SIZE;        -- OK.
          ACC1 : BOOLEAN := ACC'CONSTRAINED; -- OK.
          ACCA : INTEGER := ACC'STORAGE_SIZE;-- ERROR: ATTR.

          TSK0 : INTEGER := TSK'SIZE;        -- OK.
          TSK1 : BOOLEAN := TSK'CONSTRAINED; -- OK.

     END TEST_ATTRIBUTES;

     PACKAGE BODY TEST_ATTRIBUTES IS
     BEGIN
          FLTFLT (FLT'FIRST);                -- ERROR: 'FIRST.
          FLTFLT (FLT'LAST);                 -- ERROR: 'LAST.
     END TEST_ATTRIBUTES;

BEGIN

     NULL;

END B74207A;
