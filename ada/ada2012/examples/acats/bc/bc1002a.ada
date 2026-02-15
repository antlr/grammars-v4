-- BC1002A.ADA

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
-- CHECK THAT GENERIC PARAMETERS MAY NOT APPEAR WHERE A STATIC VALUE IS
-- REQUIRED, IN A CHOICE, INTEGER TYPE DEFINITION, ACCURACY CONSTRAINT,
-- REPRESENTATION SPECIFICATION, OR DISCRIMINANT IN AN AGGREGATE
-- GOVERNING A VARIANT PART. THE CHOICES TESTED ARE IN A VARIANT PART,
-- AN ARRAY AGGREGATE CHOICE WHICH MUST BE STATIC, AND A CASE STATEMENT.
-- NOTE THAT SOME CHOICES IN ARRAY AGGREGATES AND PARTS OF ACCURACY
-- CONSTRAINTS MAY BE DYNAMIC.

-- DAT 7/16/81
-- SPS 4/13/82
-- SPS 2/10/83
-- JBG 10/24/83
-- JRL 09/29/96  Changed upper bound of type FIX from 1.0 to 10.0.

PROCEDURE BC1002A IS

     SUBTYPE T IS BOOLEAN RANGE TRUE .. TRUE;
     SUBTYPE T1 IS INTEGER RANGE 1 .. 1;

     TYPE UA IS ARRAY (T RANGE <> ) OF T;
     SUBTYPE CA IS UA (T);


     GENERIC
          P : T;
          I : T1;
     PROCEDURE PROC;

     PROCEDURE PROC IS

          C : CONSTANT INTEGER := T'POS(P);

          TYPE REC (D : T) IS RECORD
               CASE D IS
                    WHEN P => NULL;          -- ERROR: P NOT STATIC.
                    WHEN OTHERS => NULL;
               END CASE;
          END RECORD;

          ARR1 : CONSTANT UA := (T => P);    -- OK.
          ARR2 : CONSTANT UA := (P => P);    -- OK.
          ARR3 : CONSTANT UA :=
               CA'(T => P, OTHERS => P);     -- OK.
          ARR4 : CONSTANT UA := 
               CA'(P => P, OTHERS => P);     -- ERROR: P NOT STATIC.
          ARR5 : CONSTANT STRING (C..C) :=
               (C => ' ', 2 .. 1 => ' ');    -- ERROR: C NOT STATIC.
          ARR6 : CONSTANT STRING (1 .. 2) :=
               (C => 'A', 2 => 'B');         -- ERROR: C NOT STATIC.
          ARR7 : CONSTANT STRING :=
               (C .. C + 1 => ' ');          -- OK.

          TYPE NEWINT IS RANGE I .. I;       -- ERROR: I NOT STATIC.
          TYPE NEWFLOAT IS DIGITS I;         -- ERROR: I NOT STATIC.
          TYPE NFL2 IS DIGITS 1 RANGE 1.0 ..
               FLOAT(I);                     -- ERROR: I NOT STATIC.
          TYPE NEWFIX IS DELTA
               FLOAT(I) RANGE 1.0 .. 1.0;    -- ERROR: I NOT STATIC.
          TYPE NFX2 IS DELTA 0.5 RANGE
               1.0 .. FLOAT(C);              -- ERROR: C NOT STATIC.
          TYPE FIX IS DELTA 1.0 RANGE 1.0 .. 10.0; -- OK.
          FOR FIX'SMALL USE FLOAT(I);             -- ERROR: NOT STATIC.
          B1 : T := 1.0 IN FIX(I) .. FIX(C);      -- OK.
          B3 : T := 1.0 IN FLOAT(I) .. FLOAT(C);  -- OK.

          TYPE REC2 (D:T) IS RECORD
               CASE D IS
                    WHEN TRUE => NULL;
               END CASE;
          END RECORD;

          R1 : REC2(P) := (D => TRUE);       -- OK.
          R2 : REC2 (TRUE) := (D=>P);        -- ERROR: P NOT STATIC.
          TYPE ARR IS ARRAY (INTEGER RANGE <>) OF T;
          TYPE REC3 IS RECORD
               C : ARR(1..I);                -- OK.
          END RECORD;

          TYPE ENUM IS (ALPHA);

          FOR ENUM USE (ALPHA => T1'POS(I)); -- ERROR: NOT STATIC.

     BEGIN
          CASE P IS
               WHEN P => NULL;               -- ERROR: P NOT STATIC.
               WHEN OTHERS => NULL;
          END CASE;
     END PROC;

BEGIN
     DECLARE
          PROCEDURE QQ IS NEW PROC (TRUE, 1);
     BEGIN
          NULL;
     END;
END BC1002A;
