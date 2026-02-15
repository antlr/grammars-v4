-- B48001A.ADA

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
-- CHECK THAT ILLEGAL FORMS OF ALLOCATORS ARE FORBIDDEN. IN PARTICULAR,
-- FOR ALLOCATORS OF THE FORM "NEW T", CHECK THAT T CANNOT BE:
--   A) AN UNCONSTRAINED RECORD,
--   B) PRIVATE TYPE, OR
--   C) LIMITED PRIVATE TYPE
-- HAVING DISCRIMINANTS WITHOUT DEFAULT VALUES.

-- RM  11/12/80
-- RM  01/01/82
-- VKG 01/05/83
-- SPS 02/10/83
-- EG  07/05/84

PROCEDURE  B48001A  IS

BEGIN

     -- UNCONSTRAINED RECORD

     DECLARE

          TYPE BOOLARRAY IS ARRAY(INTEGER RANGE <>) OF BOOLEAN;

          TYPE R1(DISC1 : INTEGER; DISC2 : BOOLEAN) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE R2(DISC1 : INTEGER; DISC2 : BOOLEAN) IS
               RECORD
                    X : INTEGER;
               END RECORD;

          TYPE R3(DISC1 : INTEGER; DISC2 : BOOLEAN) IS
               RECORD
                    Y : BOOLARRAY(1 .. DISC1);
               END RECORD;

          TYPE R4(DISC1 : INTEGER := 17; DISC2 : BOOLEAN := TRUE) IS
               RECORD
                    NULL;
               END RECORD;

          TYPE R5(DISC1 : INTEGER := 17; DISC2 : BOOLEAN := TRUE) IS
               RECORD
                    X : INTEGER;
               END RECORD;

          TYPE R6(DISC1 : INTEGER := 17; DISC2 : BOOLEAN := TRUE) IS
               RECORD
                    Y : BOOLARRAY(1 .. DISC1);
               END RECORD;


          SUBTYPE  R7   IS      R2 ;
          SUBTYPE  R8   IS      R6 ;
          TYPE     R9   IS  NEW R3 ;
          TYPE     R10  IS  NEW R5 ;
          SUBTYPE  R11  IS      R9 ;
          SUBTYPE  R12  IS      R10 ;
          TYPE     R13  IS  NEW R7 ;
          TYPE     R14  IS  NEW R8 ;

          TYPE  AR1  IS  ACCESS R1 ;
          TYPE  AR2  IS  ACCESS R2(3, TRUE);
          TYPE  AR3  IS  ACCESS R3 ;
          TYPE  AR4  IS  ACCESS R4 ;
          TYPE  AR5  IS  ACCESS R5 ;
          TYPE  AR6  IS  ACCESS R6 ;
          TYPE  AR7  IS  ACCESS R7 ;
          TYPE  AR8  IS  ACCESS R8 ;
          TYPE  AR9  IS  ACCESS R9 ;
          TYPE  AR10  IS  ACCESS R10 ;
          TYPE  AR11  IS  ACCESS R11 ;
          TYPE  AR12  IS  ACCESS R12 ;
          TYPE  AR13  IS  ACCESS R13 ;
          TYPE  AR14  IS  ACCESS R14 ;

          V1 : AR1 ;
          V2 : AR2 ;
          V3 : AR3 ;
          V4 : AR4 ;
          V5 : AR5 ;
          V6 : AR6 ;
          V7 : AR7 ;
          V8 : AR8 ;
          V9 : AR9 ;
          V10 : AR10 ;
          V11 : AR11 ;
          V12 : AR12 ;
          V13 : AR13 ;
          V14 : AR14 ;

     BEGIN

          V1 :=  NEW  R1 ; -- ERROR: DISCRIMINANT CONSTRAINT REQUIRED
          V2 :=  NEW  R2 ; -- ERROR: DISCRIMINANT CONSTRAINT REQUIRED
          V3 :=  NEW  R3 ; -- ERROR: DISCRIMINANT CONSTRAINT REQUIRED

          V4 :=  NEW  R4 ; -- OK
          V5 :=  NEW  R5 ; -- OK
          V6 :=  NEW  R6 ; -- OK

          V7 :=  NEW  R7 ; -- ERROR: DISCRIMINANT CONSTRAINT REQUIRED
          V8 :=  NEW  R8 ; -- OK
          V9 :=  NEW  R9 ; -- ERROR: DISCRIMINANT CONSTRAINT REQUIRED
          V10:=  NEW  R10; -- OK
          V11:=  NEW  R11; -- ERROR: DISCRIMINANT CONSTRAINT REQUIRED
          V12:=  NEW  R12; -- OK
          V13:=  NEW  R13; -- ERROR: DISCRIMINANT CONSTRAINT REQUIRED
          V14:=  NEW  R14; -- OK

     END ;   -- RECORD TYPES

     -- UNCONSTRAINED PRIVATE TYPES

     DECLARE

          PACKAGE  PACK1  IS
     
               TYPE  R1(DISC1 : INTEGER; DISC2 : BOOLEAN) IS PRIVATE;
               TYPE  R2(DISC1 : INTEGER; DISC2 : BOOLEAN) IS PRIVATE;
               TYPE  R3(DISC1 : INTEGER; DISC2 : BOOLEAN) IS PRIVATE;
               TYPE  R4(DISC1 : INTEGER := 17; DISC2 : BOOLEAN := TRUE)
                    IS PRIVATE;
               TYPE  R5(DISC1 : INTEGER := 17; DISC2 : BOOLEAN := TRUE)
                    IS PRIVATE;
               TYPE  R6(DISC1 : INTEGER := 17; DISC2 : BOOLEAN := TRUE)
                    IS PRIVATE;
     
          PRIVATE
     
               TYPE BOOLARRAY IS ARRAY(INTEGER RANGE <>) OF BOOLEAN;
     
               TYPE  R1( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    RECORD
                         NULL ;
                    END RECORD ;
     
               TYPE  R2( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    RECORD
                         X : INTEGER ;
                    END RECORD ;
     
               TYPE  R3( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    RECORD
                         Y : BOOLARRAY( 1..DISC1 );
                    END RECORD ;
     
               TYPE  R4(DISC1 : INTEGER := 17; 
                        DISC2 : BOOLEAN := TRUE) IS
                    RECORD
                         NULL ;
                    END RECORD ;
     
               TYPE  R5(DISC1 : INTEGER := 17;
                        DISC2 : BOOLEAN := TRUE) IS
                    RECORD
                         X : INTEGER ;
                    END RECORD ;

               TYPE  R6(DISC1 : INTEGER := 17;
                        DISC2 : BOOLEAN := TRUE) IS
                    RECORD
                         Y : BOOLARRAY( 1..DISC1 );
                    END RECORD ;
     
          END PACK1 ;
     
     
          TYPE  A_PACK1_R1  IS  ACCESS PACK1.R1 ;
          TYPE  A_PACK1_R2  IS  ACCESS PACK1.R2 ;
          TYPE  A_PACK1_R3  IS  ACCESS PACK1.R3(3, TRUE) ;
          TYPE  A_PACK1_R4  IS  ACCESS PACK1.R4 ;
          TYPE  A_PACK1_R5  IS  ACCESS PACK1.R5 ;
          TYPE  A_PACK1_R6  IS  ACCESS PACK1.R6 ;
     
          V1 : A_PACK1_R1 ;
          V2 : A_PACK1_R2 ;
          V3 : A_PACK1_R3 ;
          V4 : A_PACK1_R4 ;
          V5 : A_PACK1_R5 ;
          V6 : A_PACK1_R6 ;
     
     BEGIN
     
          V1 :=  NEW  PACK1.R1 ; -- ERROR: DISCRIMINANT VALUES REQUIRED
          V2 :=  NEW  PACK1.R2 ; -- ERROR: DISCRIMINANT VALUES REQUIRED
          V3 :=  NEW  PACK1.R3 ; -- ERROR: DISCRIMINANT VALUES REQUIRED
     
          V4 :=  NEW  PACK1.R4 ; -- OK
          V5 :=  NEW  PACK1.R5 ; -- OK
          V6 :=  NEW  PACK1.R6 ; -- OK
     
     END;

     -- UNCONSTRAINED LIMITED PRIVATE TYPE

     DECLARE

          PACKAGE  PACK2  IS
     
               TYPE  R1( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    LIMITED PRIVATE ;
               TYPE  R2( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    LIMITED PRIVATE ;
               TYPE  R3( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    LIMITED PRIVATE ;
               TYPE  R4(DISC1 : INTEGER := 17;
                        DISC2 : BOOLEAN := TRUE) IS
                    LIMITED PRIVATE ;
               TYPE  R5(DISC1 : INTEGER := 17;
                        DISC2 : BOOLEAN := TRUE) IS
                    LIMITED PRIVATE ;
               TYPE  R6(DISC1 : INTEGER := 17;
                        DISC2 : BOOLEAN := TRUE) IS
                    LIMITED PRIVATE ;
     
          PRIVATE
     
               TYPE BOOLARRAY IS ARRAY(INTEGER RANGE <>) OF BOOLEAN;
     
               TYPE  R1( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    RECORD
                         NULL ;
                    END RECORD ;
     
               TYPE  R2( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    RECORD
                         X : INTEGER ;
                    END RECORD ;
     
               TYPE  R3( DISC1: INTEGER ; DISC2 : BOOLEAN )  IS
                    RECORD
                         Y : BOOLARRAY( 1..DISC1 ) ;
                    END RECORD ;
     
               TYPE  R4(DISC1 : INTEGER := 17;
                        DISC2 : BOOLEAN := TRUE) IS
                    RECORD
                         NULL ;
                    END RECORD ;
     
               TYPE  R5(DISC1 : INTEGER := 17;
                        DISC2 : BOOLEAN := TRUE) IS
                    RECORD
                         X : INTEGER ;
                    END RECORD ;
     
               TYPE  R6(DISC1 : INTEGER := 17;
                        DISC2 : BOOLEAN := TRUE) IS
                    RECORD
                         Y : BOOLARRAY( 1..DISC1 ) ;
                    END RECORD ;
     
          END PACK2 ;
     
     
          TYPE  A_PACK2_R1  IS  ACCESS PACK2.R1(3, TRUE);
          TYPE  A_PACK2_R2  IS  ACCESS PACK2.R2 ;
          TYPE  A_PACK2_R3  IS  ACCESS PACK2.R3 ;
          TYPE  A_PACK2_R4  IS  ACCESS PACK2.R4 ;
          TYPE  A_PACK2_R5  IS  ACCESS PACK2.R5 ;
          TYPE  A_PACK2_R6  IS  ACCESS PACK2.R6 ;
     
          VV1 : A_PACK2_R1 ;
          VV2 : A_PACK2_R2 ;
          VV3 : A_PACK2_R3 ;
          VV4 : A_PACK2_R4 ;
          VV5 : A_PACK2_R5 ;
          VV6 : A_PACK2_R6 ;
     
     BEGIN
     
          VV1 :=  NEW  PACK2.R1 ; -- ERROR: DISCRIMINANT VALUES REQUIRED
          VV2 :=  NEW  PACK2.R2 ; -- ERROR: DISCRIMINANT VALUES REQUIRED
          VV3 :=  NEW  PACK2.R3 ; -- ERROR: DISCRIMINANT VALUES REQUIRED
          VV4 :=  NEW  PACK2.R4 ; -- OK
          VV5 :=  NEW  PACK2.R5 ; -- OK
          VV6 :=  NEW  PACK2.R6 ; -- OK
     
     END;
     
END B48001A;
