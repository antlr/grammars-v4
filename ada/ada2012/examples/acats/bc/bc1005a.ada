-- BC1005A.ADA

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
-- CHECK THAT THE IDENTIFIER OF A GENERIC UNIT CANNOT BE USED IN ITS
-- GENERIC FORMAL PART AS A:

-- CASES:
--     A) TYPEMARK.
--     B) PRIMARY IN AN EXPRESSION.
--     C) FUNCTION NAME IN FUNCTION CALL.
--     D) PREFIX OF INDEXED COMPONENT.
--     E) PREFIX OF A SELECTED COMPONENT.
--     F) DEFAULT NAME IN FORMAL SUBPROGRAM DECLARATION.
--     G) SELECTOR OF EXPANDED NAME WHOSE PREFIX DENOTES A UNIT 
--        ENCLOSING THE GENERIC DECLARATION.

-- FOR A GENERIC FUNCTION, CHECK THAT THE FUNCTION IS NOT CONSIDERED 
-- OVERLOADABLE WITHIN ITS GENERIC FORMAL PART.

-- AH  10/1/86
-- JRL 03/20/92 DELETED THE FOLLOWING CASES: FUNCTION CALL (PROCEDURE
--              CASE); PREFIX OF A SLICE; PREFIX OF AN ATTRIBUTE.
-- JRL 04/02/92 REMOVED UNNECESSARY REFERENCES TO REPORT AND IDENT_INT.

PROCEDURE BC1005A IS
     PACKAGE PKG IS
          TYPE TM IS RANGE 1..10;
          TYPE ARR1 IS ARRAY (1..5) OF INTEGER;

          TYPE REC_TYPE IS
               RECORD
                    REC_OBJ : INTEGER := 2 ;
               END RECORD;

          PRIM : CONSTANT INTEGER := 5;
          ARR_OBJ1 : ARR1 := (1..5 => 1);
          REC : REC_TYPE;
          OBJ_10 : INTEGER := 2 ;

          FUNCTION FUNC2 RETURN INTEGER;

          FUNCTION FUNC3 RETURN INTEGER;

          FUNCTION FUNC4 (X : FLOAT) RETURN INTEGER;
     END PKG;
     USE PKG;

     GENERIC
          WITH FUNCTION GEN_1 RETURN TM;             -- ERROR: A.
     PACKAGE TM IS END TM;

     GENERIC 
          GEN2_OBJ : INTEGER := PRIM + 5 ; -- ERROR: B.
     PROCEDURE PRIM;

     GENERIC
          GEN4_OBJ : INTEGER := FUNC2;               -- ERROR: C.
     PACKAGE FUNC2 IS END FUNC2;

     GENERIC
          GEN5_OBJ : INTEGER := ARR_OBJ1(3);         -- ERROR: D.
     PACKAGE ARR_OBJ1 IS END ARR_OBJ1;

     GENERIC
          GEN7_OBJ : INTEGER := REC.REC_OBJ;         -- ERROR: E.
     FUNCTION REC RETURN INTEGER;

     GENERIC
          WITH FUNCTION GEN_FUNC RETURN INTEGER 
               IS FUNC3;                             -- ERROR: F.
     PROCEDURE FUNC3;

     PACKAGE EN IS
          P_OBJ : INTEGER := 0 ;
          GENERIC
               GEN9_OBJ : INTEGER := EN.P_OBJ;       -- ERROR: G.
          PROCEDURE EN;
     END EN;

     PACKAGE BODY EN IS
          PROCEDURE EN IS
          BEGIN
               NULL;
          END EN;
     END EN;

     GENERIC
          GEN10_OBJ : INTEGER := FUNC4(1.0);     -- ERROR: FUNCTION NOT
                                                 --        OVERLOADABLE.
     FUNCTION FUNC4 (X : INTEGER) RETURN INTEGER;

     PROCEDURE PRIM IS
     BEGIN
          NULL;
     END PRIM;

     FUNCTION REC RETURN INTEGER IS
     BEGIN
          RETURN 1 ;
     END REC;

     PROCEDURE FUNC3 IS
     BEGIN
          NULL;
     END FUNC3;

     FUNCTION FUNC4 (X : INTEGER) RETURN INTEGER IS
     BEGIN
          RETURN 4 ;
     END FUNC4;

     PACKAGE BODY PKG IS
          FUNCTION FUNC2 RETURN INTEGER IS
          BEGIN
               RETURN 3 ;
          END FUNC2;

          FUNCTION FUNC3 RETURN INTEGER IS
          BEGIN
               RETURN 1 ;
          END FUNC3;
 
          FUNCTION FUNC4 (X : FLOAT) RETURN INTEGER IS
          BEGIN
               RETURN 4 ;
          END FUNC4;
     END PKG;

BEGIN
     NULL;
END BC1005A;
