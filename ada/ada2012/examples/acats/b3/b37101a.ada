-- B37101A.ADA

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
-- CHECK THAT DISCRIMINANTS MUST BE A DISCRETE TYPE.
-- CHECK THAT ALL OR NO DISCRIMINANTS MAY BE INITIALIZED.
-- CHECK THAT A DISCRIMNANT OF THE TYPE BEING DECLARED IS NOT
-- ALLOWED IN AN EXPRESSION FOR A RANGE CONSTRAINT, FIXED POINT
-- CONSTRAINT, OR FLOATING POINT CONSTRAINT.

-- DAT 5/18/81
-- RJW 1/28/86 - RENAMED FROM B37101A.ADA. ADDED A CHECK THAT A 
--               DISCRIMININANT OF THE TYPE BEING DECLARED IS NOT 
--               ALLOWED AS A RANGE CONSTRAINT, FIXED POINT CONSTRAINT,
--               OR FLOATING POINT CONSTRAINT.

PROCEDURE B37101A IS
     
     TYPE ACC_STRING IS ACCESS STRING(1..20);

     FUNCTION F (S : ACC_STRING) RETURN INTEGER IS
     BEGIN
          RETURN 1;
     END F;                                        
BEGIN

BLOCK1:
     DECLARE 
          TYPE F IS DELTA 1.0 RANGE 1.0 .. 1.0;
          TYPE G IS DIGITS 1 RANGE 1.0 .. 1.0;
          TYPE A IS ARRAY(1..1) OF BOOLEAN;
          TYPE R IS RECORD B : BOOLEAN; END RECORD;
          PACKAGE PK IS
               TYPE P IS PRIVATE;
               TYPE L IS LIMITED PRIVATE;
               TASK TYPE T;
          PRIVATE
               TYPE P IS (TRUE, FALSE);
               TYPE L IS RANGE 1 .. 1;
          END PK;
          TYPE X1 (D : F) IS RECORD         -- ERROR: F.
               NULL; END RECORD;
          TYPE X2 (D : G) IS RECORD         -- ERROR: G.
               NULL; END RECORD;
          TYPE X3 (D : A) IS RECORD         -- ERROR: A.
               NULL; END RECORD;
          TYPE X4 (D : R) IS RECORD         -- ERROR: R.
               NULL; END RECORD;
          USE PK;
          TYPE X5 (D : P) IS RECORD         -- ERROR: P.
               NULL; END RECORD;
          TYPE X6 (D : L) IS RECORD         -- ERROR: L.
               NULL; END RECORD;
          O : T;                            -- OK.
          TYPE X7 (D : T) IS RECORD         -- ERROR: T.
               NULL; END RECORD;
          PACKAGE BODY PK IS
               TASK BODY T IS
               BEGIN NULL; END T;
          END PK;
     BEGIN
          NULL;
     END BLOCK1;

BLOCK2:
     DECLARE
          SUBTYPE B IS BOOLEAN RANGE TRUE .. TRUE;
          SUBTYPE I IS INTEGER RANGE 0 .. 0;
          T : B := TRUE;
          TYPE R1 (D : B; E : I := 0) IS    -- ERROR: :=.
               RECORD NULL; END RECORD;
          TYPE R2 (D : B := T; E : I) IS    -- ERROR: NO :=.
               RECORD NULL; END RECORD;
     BEGIN
          NULL;
     END BLOCK2;

BLOCK3:
     DECLARE
          
          TYPE FIXED IS DELTA 0.1 RANGE 1.0 .. 3.0;

          TYPE R (DISC : INTEGER) IS RECORD
               C1 : INTEGER RANGE DISC .. 3; -- ERROR: DISC IN RANGE 
                                             --        CONSTRAINT.
               C2 : INTEGER 
                    RANGE 1 .. F (NEW STRING(1 .. DISC) );  -- ERROR: 
                                                   -- DISC IN RANGE 
                                                   -- CONSTRAINT.
               C3 : FIXED DELTA 1.0 
                    RANGE FIXED (DISC) .. 3.0; -- ERROR: DISC IN FIXED 
                                               --        CONSTRAINT.  
               C4 : FLOAT DIGITS 1 
                    RANGE FLOAT (DISC) .. 3.0; -- ERROR: DISC IN FLOAT
                                               --        CONSTRAINT.
          END RECORD;

          TYPE RR (DISC : INTEGER) IS RECORD 
               NULL; 
          END RECORD;

          RR1 : RR (1);

          TYPE S (DISC1 : INTEGER) IS RECORD
               C1 : INTEGER RANGE RR1.DISC .. 3; -- OK.
               C2 : INTEGER RANGE 1 .. F (NEW STRING (1 .. RR1.DISC) );
                                             -- OK.
               C3 : FIXED DELTA 1.0 RANGE FIXED (RR1.DISC) .. 3.0;
                                             -- OK.
               C4 : FLOAT DIGITS 1 RANGE FLOAT (RR1.DISC) .. 3.0;
                                             -- OK.
          END RECORD;

     BEGIN
          NULL;
     END BLOCK3;

END B37101A;                                
