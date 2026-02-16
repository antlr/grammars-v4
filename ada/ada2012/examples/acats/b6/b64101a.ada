-- B64101A.ADA

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

--        (F) A QUALIFIED EXPRESSION CONTAINING ONLY A VARIABLE NAME.

--        (G) AN ALLOCATOR.

--        (H) AN EXPRESSION CONTAINING AN OPERATOR.

-- DAS  1/28/81
-- SPS 11/3/82
-- JBG 7/25/83
-- JBG 10/14/83

PROCEDURE B64101A IS

     TYPE ENUMTYPE IS (ENUM0, ENUM1, ENUM2);

     TYPE MATRIX IS ARRAY (1..2,1..2) OF INTEGER;
     SUBTYPE INT IS INTEGER RANGE 1..10 ;
     TYPE ARR IS ARRAY (INT RANGE <>) OF INTEGER ;
     TYPE RECTYPE (X : INT) IS
          RECORD
               A : ARR (1..X);
          END RECORD;

     TYPE PTR IS ACCESS MATRIX;

     PACKAGE PKG IS
          TYPE PRIV IS PRIVATE;
          CP3  : CONSTANT PRIV;
          PROCEDURE PP1 (X : IN OUT PRIV);
          PROCEDURE PP2 (X : OUT PRIV);
          FUNCTION FP RETURN PRIV;
     PRIVATE
          TYPE PRIV IS NEW INTEGER;
          CP3  : CONSTANT PRIV     := 3;
     END PKG;

     I1,I2     : INTEGER      := 3;
     E1,E2     : ENUMTYPE     := ENUM1;
     M1,M2     : MATRIX       := ((1,0),(0,1));
     R1,R2     : RECTYPE(3)   := (3,(1,2,3));
     A1,A2     : PTR          := NULL;
     P1,P2     : PKG.PRIV;
     CI3       : CONSTANT INTEGER := 3;
     C3        : CONSTANT       := 3;


     PROCEDURE PI1 (X : IN OUT INTEGER) IS
     BEGIN
          NULL;
     END PI1;

     PROCEDURE PI2 (X : OUT INTEGER) IS
     BEGIN
          NULL;
     END PI2;

     FUNCTION FI RETURN INTEGER IS
     BEGIN
          RETURN 1;
     END FI;


     PROCEDURE PE1 (X : IN OUT ENUMTYPE) IS
     BEGIN
          NULL;
     END PE1;

     PROCEDURE PE2 (X : OUT ENUMTYPE) IS
     BEGIN
          NULL;
     END PE2;

     FUNCTION FE RETURN ENUMTYPE IS
     BEGIN
          RETURN ENUM1;
     END FE;


     PROCEDURE PM1 (X : IN OUT MATRIX) IS
     BEGIN
          NULL;
     END PM1;

     PROCEDURE PM2 (X : OUT MATRIX) IS
     BEGIN
          NULL;
     END PM2;

     FUNCTION FM RETURN MATRIX IS
     BEGIN
          RETURN ((1,1),(1,1));
     END FM;


     PROCEDURE PR1 (X : IN OUT RECTYPE) IS
     BEGIN
          NULL;
     END PR1;

     PROCEDURE PR2 (X : OUT RECTYPE) IS
     BEGIN
          NULL;
     END PR2;

     FUNCTION FR RETURN RECTYPE IS
     BEGIN
          RETURN (3,(1,2,3));
     END FR;


     PROCEDURE PA1 (X : IN OUT PTR) IS
     BEGIN
          NULL;
     END PA1;

     PROCEDURE PA2 (X : OUT PTR) IS
     BEGIN
          NULL;
     END PA2;

     FUNCTION FA RETURN PTR IS
     BEGIN
          RETURN NEW MATRIX;
     END FA;


     PACKAGE BODY PKG IS

          PROCEDURE PP1 (X : IN OUT PRIV) IS
          BEGIN
               NULL;
          END PP1;

          PROCEDURE PP2 (X : OUT PRIV) IS
          BEGIN
               NULL;
          END PP2;

          FUNCTION FP RETURN PRIV IS
          BEGIN
               RETURN 1;
          END FP;

     END PKG;

BEGIN

     DECLARE
          PROCEDURE P (X : IN INTEGER) IS
          BEGIN
               PI1 (X);       -- ERROR: (A).
               NULL;
               PI2 (X);       -- ERROR: (A).
               NULL;
          END P;
     BEGIN
          NULL;
     END;

     PI1 (CI3);               -- ERROR: (A).
     NULL;
     PI2 (CI3);               -- ERROR: (A).
     NULL;
     PI1 (C3);                -- ERROR: (A).
     NULL;
     PI2 (C3);                -- ERROR: (A).
     NULL;
     PKG.PP1 (PKG.CP3);       -- ERROR: (A).
     NULL;
     PKG.PP2 (PKG.CP3);       -- ERROR: (A).
     NULL;
     PI1 (3);                 -- ERROR: (A).
     NULL;
     PI2 (3);                 -- ERROR: (A).
     NULL;
     PE1 (ENUM2);             -- ERROR: (A).
     NULL;
     PE2 (ENUM2);             -- ERROR: (A).
     NULL;
     FOR I IN 1..10 LOOP
          PI1 (I);            -- ERROR: (A).
          NULL;
          PI2 (I);            -- ERROR: (A).
          NULL;
     END LOOP;
     NULL;
     PI1 (R1.X);              -- ERROR: (A).
     NULL;
     PI2 (R1.X);              -- ERROR: (A).
     NULL;
     PA1(NULL);               -- ERROR: (A).
     NULL;
     PA2(NULL);               -- ERROR: (A).
     NULL;
     PI1 (FR.X);              -- ERROR: (A).
     NULL;
     PI2 (FR.X);              -- ERROR: (A).
     NULL;

     PI1 ((I1));              -- ERROR: (B).
     NULL;
     PI2 ((I1));              -- ERROR: (B).
     NULL;
     PI1 (INTEGER(I1));       -- OK.
     PI2 (INTEGER(I1));       -- OK.
     PI1 (INTEGER((I1)));     -- ERROR: (B1).
     NULL;
     PI2 (INTEGER((I1)));     -- ERROR: (B1).

     PR1 (FR);                -- ERROR: (C).
     NULL;
     PR2 (FR);                -- ERROR: (C).
     NULL;
     PM1 (FM);                -- ERROR: (C).
     NULL;
     PM2 (FM);                -- ERROR: (C).
     NULL;
     PKG.PP1 (PKG.FP);        -- ERROR: (C).
     NULL;
     PKG.PP2 (PKG.FP);        -- ERROR: (C).
     NULL;
     PI1 (FI);                -- ERROR: (C).
     NULL;
     PI2 (FI);                -- ERROR: (C).
     NULL;
     PE1 (FE);                -- ERROR: (C).
     NULL;
     PE2 (FE);                -- ERROR: (C).
     NULL;
     PA1 (FA);                -- ERROR: (C).
     NULL;
     PA2 (FA);                -- ERROR: (C).
     NULL;

     PI1 (MATRIX'LENGTH(1));  -- ERROR: (D).
     NULL;
     PI2 (MATRIX'LENGTH(1));  -- ERROR: (D).
     NULL;
     PE1 (ENUMTYPE'LAST);     -- ERROR: (D).
     NULL;
     PE2 (ENUMTYPE'SUCC(E1)); -- ERROR: (D).
     NULL;

     PR1 ((3,(1,2,3)));       -- ERROR: (E).
     NULL;
     PR2 ((3,(1,2,3)));       -- ERROR: (E).
     NULL;
     PM1 (((1,0),(0,1)));     -- ERROR: (E).
     NULL;
     PM2 (((1,0),(0,1)));     -- ERROR: (E).
     NULL;
     PM1 (((I1,I2),(I2,I1))); -- ERROR: (E).
     NULL;
     PM2 (((I1,I2),(I2,I1))); -- ERROR: (E).
     NULL;

     PI1 (INTEGER'(C3));      -- ERROR: (F).
     NULL;
     PI2 (INTEGER'(C3));      -- ERROR: (F).
     NULL;
     PM1 (MATRIX'(M1));       -- ERROR: (F).
     NULL;
     PM2 (MATRIX'(M2));       -- ERROR: (F).
     NULL;

     PA1 (NEW MATRIX);        -- ERROR: (G).
     NULL;
     PA2 (NEW MATRIX);        -- ERROR: (G).
     NULL;

     PI1 (I1 + I2);           -- ERROR: (H).
     NULL;
     PI2 (I1 + I2);           -- ERROR: (H).
     NULL;

END B64101A;
