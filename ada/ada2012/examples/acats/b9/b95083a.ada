-- B95083A.ADA

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
-- CHECK THAT THE EXPRESSION CORRESPONDING TO AN OUT OR AN IN OUT
--   PARAMETER CANNOT BE:

--        (A) A CONSTANT, INCLUDING AN IN FORMAL PARAMETER, AN
--            ENUMERATION LITERAL, A LOOP PARAMETER, A RECORD
--            DISCRIMINANT, NULL, OR A NUMBER NAME.

--        (B) A PARENTHESIZED VARIABLE.
--        (B1) A TYPE CONVERSION WITH A PARENTHESIZED VARIABLE.

--        (C) A FUNCTION RETURNING A RECORD, ARRAY, PRIVATE, SCALAR,
--            OR ACCESS TYPE.

--        (D) AN ATTRIBUTE.

--        (E) AN AGGREGATE, EVEN ONE CONSISTING ONLY OF VARIABLES.

--        (G) AN ALLOCATOR.

--        (H) AN EXPRESSION CONTAINING AN OPERATOR.

-- JWC 7/17/85
-- DTN 11/19/91  DELETED SUBPART (F).

PROCEDURE B95083A IS

     TYPE ENUMTYPE IS (ENUM0, ENUM1, ENUM2);

     TYPE MATRIX IS ARRAY (1..2, 1..2) OF INTEGER;

     SUBTYPE INT IS INTEGER RANGE 1..10;
     TYPE ARR IS ARRAY (INT RANGE <>) OF INTEGER;

     TYPE RECTYPE (X : INT) IS
          RECORD
               A : ARR (1..X);
          END RECORD;

     TYPE PTR IS ACCESS MATRIX;

     PACKAGE PKG IS
          TYPE PRIV IS PRIVATE;
          CP3 : CONSTANT PRIV;
          FUNCTION FP RETURN PRIV;
          TASK T2 IS
               ENTRY EP1 (X : IN OUT PRIV);
               ENTRY EP2 (X : OUT PRIV);
          END T2;
     PRIVATE
          TYPE PRIV IS NEW INTEGER;
          CP3 : CONSTANT PRIV := 3;
     END PKG;

     I1,I2     : INTEGER      := 3;
     E1,E2     : ENUMTYPE     := ENUM1;
     R1,R2     : RECTYPE(3)   := (3,(1,2,3));
     A1,A2     : PTR          := NULL;
     P1,P2     : PKG.PRIV;
     CI3       : CONSTANT INTEGER := 3;
     C3        : CONSTANT     := 3;

     FUNCTION FI RETURN INTEGER IS
     BEGIN
          RETURN 1;
     END FI;

     FUNCTION FE RETURN ENUMTYPE IS
     BEGIN
          RETURN ENUM1;
     END FE;

     FUNCTION FM RETURN MATRIX IS
     BEGIN
          RETURN ((1,1),(1,1));
     END FM;

     FUNCTION FR RETURN RECTYPE IS
     BEGIN
          RETURN (3,(1,2,3));
     END FR;

     FUNCTION FA RETURN PTR IS
     BEGIN
          RETURN NEW MATRIX;
     END FA;

     TASK T1 IS
          ENTRY EI1 (X : IN OUT INTEGER);
          ENTRY EI2 (X : OUT INTEGER);
          ENTRY EE1 (X : IN OUT ENUMTYPE);
          ENTRY EE2 (X : OUT ENUMTYPE);
          ENTRY EM1 (X : IN OUT MATRIX);
          ENTRY EM2 (X : OUT MATRIX);
          ENTRY ER1 (X : IN OUT RECTYPE);
          ENTRY ER2 (X : OUT RECTYPE);
          ENTRY EA1 (X : IN OUT PTR);
          ENTRY EA2 (X : OUT PTR);
     END T1;

     TASK BODY T1 IS
     BEGIN
          NULL;
     END T1;

     PACKAGE BODY PKG IS

          FUNCTION FP RETURN PRIV IS
          BEGIN
               RETURN 1;
          END FP;

          TASK BODY T2 IS
          BEGIN
               NULL;
          END T2;

     END PKG;

BEGIN

     DECLARE
          TASK T3 IS
               ENTRY E (X : IN INTEGER);
          END T3;

          TASK BODY T3 IS
          BEGIN
               ACCEPT E (X : IN INTEGER) DO
                    T1.EI1 (X);  -- ERROR: (A).
                    NULL;
                    T1.EI2 (X);  -- ERROR: (A).
                    NULL;
               END E;
          END T3;

     BEGIN
          NULL;
     END;

     T1.EI1 (CI3);               -- ERROR: (A).
     NULL;
     T1.EI2 (CI3);               -- ERROR: (A).
     NULL;
     T1.EI1 (C3);                -- ERROR: (A).
     NULL;
     T1.EI2 (C3);                -- ERROR: (A).
     NULL;
     PKG.T2.EP1 (PKG.CP3);       -- ERROR: (A).
     NULL;
     PKG.T2.EP2 (PKG.CP3);       -- ERROR: (A).
     NULL;
     T1.EI1 (3);                 -- ERROR: (A).
     NULL;
     T1.EI2 (3);                 -- ERROR: (A).
     NULL;
     T1.EE1 (ENUM2);             -- ERROR: (A).
     NULL;
     T1.EE2 (ENUM2);             -- ERROR: (A).
     NULL;
     FOR I IN 1..10 LOOP
          T1.EI1 (I);            -- ERROR: (A).
          NULL;
          T1.EI2 (I);            -- ERROR: (A).
          NULL;
     END LOOP;
     NULL;
     T1.EI1 (R1.X);              -- ERROR: (A).
     NULL;
     T1.EI2 (R1.X);              -- ERROR: (A).
     NULL;
     T1.EA1 (NULL);              -- ERROR: (A).
     NULL;
     T1.EA2 (NULL);              -- ERROR: (A).
     NULL;
     T1.EI1 (FR.X);              -- ERROR: (A).
     NULL;
     T1.EI2 (FR.X);              -- ERROR: (A).
     NULL;

     T1.EI1 ((I1));              -- ERROR: (B).
     NULL;
     T1.EI2 ((I1));              -- ERROR: (B).
     NULL;
     T1.EI1 (INTEGER(I1));       -- OK.
     T1.EI2 (INTEGER(I1));       -- OK.
     T1.EI1 (INTEGER((I1)));     -- ERROR: (B1).
     NULL;
     T1.EI2 (INTEGER((I1)));     -- ERROR: (B1).

     T1.ER1 (FR);                -- ERROR: (C).
     NULL;
     T1.ER2 (FR);                -- ERROR: (C).
     NULL;
     T1.EM1 (FM);                -- ERROR: (C).
     NULL;
     T1.EM2 (FM);                -- ERROR: (C).
     NULL;
     PKG.T2.EP1 (PKG.FP);        -- ERROR: (C).
     NULL;
     PKG.T2.EP2 (PKG.FP);        -- ERROR: (C).
     NULL;
     T1.EI1 (FI);                -- ERROR: (C).
     NULL;
     T1.EI2 (FI);                -- ERROR: (C).
     NULL;
     T1.EE1 (FE);                -- ERROR: (C).
     NULL;
     T1.EE2 (FE);                -- ERROR: (C).
     NULL;
     T1.EA1 (FA);                -- ERROR: (C).
     NULL;
     T1.EA2 (FA);                -- ERROR: (C).
     NULL;

     T1.EI1 (MATRIX'LENGTH(1));  -- ERROR: (D).
     NULL;
     T1.EI2 (MATRIX'LENGTH(1));  -- ERROR: (D).
     NULL;
     T1.EE1 (ENUMTYPE'LAST);     -- ERROR: (D).
     NULL;
     T1.EE2 (ENUMTYPE'SUCC(E1)); -- ERROR: (D).
     NULL;

     T1.ER1 ((3,(1,2,3)));       -- ERROR: (E).
     NULL;
     T1.ER2 ((3,(1,2,3)));       -- ERROR: (E).
     NULL;
     T1.EM1 (((1,0),(0,1)));     -- ERROR: (E).
     NULL;
     T1.EM2 (((1,0),(0,1)));     -- ERROR: (E).
     NULL;
     T1.EM1 (((I1,I2),(I2,I1))); -- ERROR: (E).
     NULL;
     T1.EM2 (((I1,I2),(I2,I1))); -- ERROR: (E).
     NULL;

     T1.EA1 (NEW MATRIX);        -- ERROR: (G).
     NULL;
     T1.EA2 (NEW MATRIX);        -- ERROR: (G).
     NULL;

     T1.EI1 (I1 + I2);           -- ERROR: (H).
     NULL;
     T1.EI2 (I1 + I2);           -- ERROR: (H).
     NULL;

END B95083A;
