-- BC3102B.ADA

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
-- CHECK THAT A GENERIC ACTUAL IN OUT PARAMETER CANNOT BE:
-- A CONSTANT, GENERIC IN PARAMETER, SUBPROGRAM IN OR OUT PARAMETER,
-- LOOP PARAMETER, A NUMBER DECLARED IN A NUMBER DECLARATION,
-- A PARENTHESIZED VARIABLE, AN UNDEREFERENCED FUNCTION
-- RESULT OF SCALAR, ARRAY, RECORD, ACCESS, OR PRIVATE TYPE.
-- ALSO, IT CANNOT BE A SLICED FUNCTION RESULT, AN ATTRIBUTE,
-- AN AGGREGATE OF VARIABLES, A QUALIFIED VARIABLE, AN ALLOCATOR,
-- A DISCRIMINANT OF A RECORD, AN EXPRESSION WITH AN OPERATOR,
-- OR A VARIABLE IN A TYPE CONVERSION.

-- NOTE:  THE DIFFERENCE BETWEEN BC3102A-AB AND BC3102B-B IS
-- THE DECLARATION OF THE FUNCTION FUNC.  IN BC3102A-AB FUNC
-- HAS AN ARGUMENT.  IN BC3102B-B, FUNC IS PARAMETERLESS.

-- DAT 9/18/81
-- SPS 4/28/82
-- JRK 3/26/84
-- PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

WITH SYSTEM; USE SYSTEM;
PROCEDURE BC3102B IS

     GENERIC
          VAR : IN OUT INTEGER;
     PACKAGE PI IS END PI;

     GENERIC 
          TYPE T IS PRIVATE;
          VAR : IN OUT T;
     PACKAGE P IS END P;

     C : CONSTANT INTEGER := 4;
     PACKAGE Z1 IS NEW PI (C);                -- ERROR: CONSTANT.
     N : CONSTANT := 4;
     PACKAGE Z2 IS NEW PI (N);                -- ERROR: NUMBER.
     V : INTEGER := 4;
     PACKAGE Z3 IS NEW PI ((V));              -- ERROR: ()'S.
     TYPE REC IS RECORD 
          C : INTEGER;
     END RECORD;
     PACKAGE Z4 IS NEW PI (ABS (V));            -- ERROR: ABS.
     PACKAGE Z5 IS NEW PI (REC'SIZE);           -- ERROR: ATTRIBUTE.
     PACKAGE Z6 IS NEW P (ADDRESS, V'ADDRESS);  -- ERROR: ATTRIBUTE.
     PACKAGE Z7 IS NEW PI (INTEGER'(V));        -- ERROR: QUALIFICATION.
     PACKAGE Z8 IS NEW P (REC, (C => V));       -- ERROR: AGGREGATE.
     TYPE AI IS ACCESS INTEGER;
     PACKAGE Z9 IS NEW P (AI, NEW INTEGER'(7)); -- ERROR: ALLOCATOR.
     TYPE REC2 (D : INTEGER := 1) IS RECORD NULL; END RECORD;
     R2 : REC2;
     PACKAGE ZA IS NEW PI (R2.D);             -- ERROR: DISCRIMINANT.
     PACKAGE ZB IS NEW PI (+V);               -- ERROR: EXPRESSION.
     PACKAGE ZC IS NEW PI (0+V);              -- ERROR: EXPRESSION.
     R : REC;
     PACKAGE ZD IS NEW PI (R.C);              -- OK.
     CR : CONSTANT REC := (C => 4);
     PACKAGE ZE IS NEW PI (CR.C);             -- ERROR: CONSTANT.
     TYPE DI IS NEW INTEGER;
     PACKAGE ZF IS NEW P (DI, DI(V));         -- ERROR: TYPE CONVERSION.

     VDI : DI := 5;
     PACKAGE ZFA IS NEW P (INTEGER, INTEGER(VDI));-- ERROR: CONV.
     TYPE ARRI IS ARRAY (1..1) OF INTEGER;
     VAI : ARRI;
     PACKAGE ZG IS NEW PI (VAI (1));          -- OK.
     CAI : CONSTANT ARRI := (1 => 1);
     PACKAGE ZH IS NEW PI (CAI (1));          -- ERROR: CONSTANT.
     PACKAGE ZI IS NEW P (ARRI, CAI (1..1));  -- ERROR: CONSTANT.
     PACKAGE ZJ IS NEW P (ARRI, VAI (1..1));  -- OK.
     
     GENERIC
          TYPE T IS PRIVATE;
          V : IN T;
     FUNCTION FUNC RETURN T;

     GENERIC
          TYPE T IS PRIVATE;
          V : IN T;
          V1 : T;
     PROCEDURE PROC (X : IN T; Y : IN OUT T; Z : OUT T);

     FUNCTION FUNC RETURN T IS
     BEGIN RETURN V; END FUNC;

     PROCEDURE PROC (X : IN T; Y : IN OUT T; Z : OUT T) IS
          PACKAGE Z1 IS NEW P (T, V);         -- ERROR: V 
                                              -- GENERIC IN PARM.
          PACKAGE Z2 IS NEW P (T, V1);        -- ERROR: V1
                                              -- GENERIC IN PARM.
          PACKAGE Z3 IS NEW P (T, X);         -- ERROR: X SUBPROGRAM
                                              -- IN PARM.
          PACKAGE Z4 IS NEW P (T, Y);         -- OK.
     BEGIN
          FOR I IN 1 .. 1 LOOP
               DECLARE
                    PACKAGE Z1 IS NEW PI (I); -- ERROR: LOOP PARM.
               BEGIN
                    NULL;
               END;
          END LOOP;
     END PROC;

     PACKAGE PPP IS
          FUNCTION FI IS NEW FUNC (INTEGER, 4);
          PACKAGE Z1 IS NEW PI (FI );         -- ERROR: UNDEREFERENCED
                                              -- FUNCTION RESULT.
          SUBTYPE ST IS STRING (1.. 1);
          FUNCTION FA IS NEW FUNC (ST, "Z");
          PACKAGE Z2 IS NEW P 
               (ST, FA);                      -- ERROR: UNDEREFERENCED
                                              -- FUNCTION RESULT.
          PACKAGE Z3 IS NEW P
               (ST, FA(1..1));                -- ERROR: SLICE OF AN
                                              -- UNDEREFERENCED FUNCT.
                                              -- RESULT.
          PACKAGE Z4 IS NEW P
               (CHARACTER, FA (1));           -- ERROR: UNDEREFERENCED
                                              -- FUNCTION RESULT.
          FUNCTION F6 IS NEW FUNC (REC, CR);
          PACKAGE Z5 IS NEW P (REC, F6);      -- ERROR: UNDEREFERENCED
                                              -- FUNCTION RESULT.

          PACKAGE Z6 IS NEW PI (F6.C);        -- ERROR: UNDEREFERENCED
                                              -- FUNCTION RESULT.

          FUNCTION FAI IS NEW FUNC (AI, NEW INTEGER'(7));

          PACKAGE Z7 IS NEW P (AI, FAI);      -- ERROR: UNDEREFERENCED
                                              -- FUNCTION RESULT.
          PACKAGE Z8 IS NEW PI (FAI.ALL);     -- OK.
          TYPE AR IS ACCESS REC;
          FUNCTION FAR IS NEW FUNC (AR, NEW REC);
          PACKAGE Z9 IS NEW P (AR, FAR);      -- ERROR: UNDEREFERENCED
                                              -- FUNCTION RESULT.
          PACKAGE Z10 IS NEW P (REC, FAR.ALL);-- OK.
          PACKAGE ZA IS NEW PI (FAR.C);       -- OK.
          TYPE AS IS ACCESS ST;
          FUNCTION FSTR IS NEW FUNC (AS, NEW ST);
          PACKAGE ZB IS NEW P (AS, FSTR);     -- ERROR: UNDEREFERENCED
                                              -- FUNCTION RESULT.
          PACKAGE ZC IS NEW P (ST,
               FSTR.ALL);                     -- OK.
          PACKAGE ZG IS NEW P (CHARACTER,
               FSTR (1));                     -- OK.
          PACKAGE ZD IS NEW P (CHARACTER,
               FSTR.ALL(1));                  -- OK.
          PACKAGE ZE IS NEW P (ST,
               FSTR(1..1));                   -- OK.
          PACKAGE ZF IS NEW P (ST,
               FSTR  .ALL(1..1)(1..1));       -- OK.
          PACKAGE PVT IS
               TYPE PP IS PRIVATE;
               CPP : CONSTANT PP;
          PRIVATE
               TYPE PP IS RANGE 1 .. 1;
               CPP : CONSTANT PP := 1;
          END PVT;
          USE PVT;

          FUNCTION FPP IS NEW FUNC (PP, CPP);
          PACKAGE ZZ IS NEW P (PP, FPP);      -- ERROR: NOT VAR.
          PACKAGE ZX IS NEW P (PP, CPP);      -- ERROR: NOT VAR.

     END;
BEGIN
     NULL;
END BC3102B;
