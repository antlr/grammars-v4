-- B45661A.ADA

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
-- CHECK THAT NON-BOOLEAN ARGUMENTS TO  'NOT' 
--    ARE FORBIDDEN.

-- RM   30 OCTOBER 1980
-- TBN 10/21/85     RENAMED FROM B45402A.ADA.  ADDED MULTI-DIMENSIONAL
--                  ARRAY OF BOOLEAN CASE.

PROCEDURE  B45661A  IS

     TYPE  BOOL_REC_3  IS
          RECORD
               P , Q , R : BOOLEAN ;
          END RECORD ;

     TYPE  BOOL_REC_1  IS
          RECORD
               S : BOOLEAN ;
          END RECORD ;

     TYPE BOOL_MATRIX IS ARRAY (1 .. 2, 1 .. 2) OF BOOLEAN;

     INTEGER_VAR  ,IV :  INTEGER   := 11 ;
     CHARACTER_VAR,CV :  CHARACTER := 'A' ;
     INT_ARRAY ,   IA :  ARRAY( 1..3 ) OF INTEGER := ( 1 , 2 , 3 ) ;
     CHAR_STRING , CS : STRING( 1..4 ) := "ABCD" ;
     BOOL_REC_3_VAR,BV3: BOOL_REC_3 := ( TRUE , TRUE , TRUE ) ;
     BOOL_REC_1_VAR,BV1: BOOL_REC_1 := ( S => TRUE ) ;
     BOOL_MAT_VAR, BMV : BOOL_MATRIX := ((TRUE, TRUE), (TRUE, TRUE));

BEGIN

-- 28 CASES ( 7 :  TYPES ,
--            2 :  TYPE OF RESULT IS BOOLEAN OR SAME AS THE OPERAND'S
--            2 :  LITERAL/VARIABLE ) , OF WHICH THE FOLLOWING 14
--     ARE COVERED:  ALL THE COMBINATIONS OF VALUES OF THE FIRST
--     FACTOR WITH VALUES OF THE THIRD FACTOR, WHILE
--     THE SECOND FACTOR IS 'BOOLEAN' HALF THE TIME AND 'SAME AS INPUT'
--     HALF THE TIME.


     IF  NOT( INTEGER_VAR )       -- ERROR: INTEGER (VAR.) ARG. TO 'NOT'
     THEN  NULL ;
     END IF;

     IF  NOT( 1 )                 -- ERROR: INTEGER (LIT.) ARG. TO 'NOT'
     THEN  NULL ;
     END IF;

     IF  NOT( 'W' )             -- ERROR: CHARACTER (LIT.) ARG. TO 'NOT'
     THEN  NULL ;
     END IF;

     IF  NOT( CHARACTER_VAR )            -- ERROR: CHARACTER (VAR.) ARG.
     THEN  NULL ;
     END IF;

     IF  NOT( ( S => TRUE) )            -- ERROR: BOOL_REC_1 (LIT.) ARG.
     THEN  NULL ;
     END IF;

     IF  NOT( BOOL_REC_1_VAR )          -- ERROR: BOOL_REC_1 (VAR.) ARG.
     THEN  NULL ;
     END IF;

     CS := NOT( CHAR_STRING ) ;        -- ERROR: ARRAY (CHAR, VAR.) ARG.
     CS := NOT( "EFGH" ) ;             -- ERROR: ARRAY (CHAR, LIT.) ARG.
     IA := NOT( ( 7 , 8 , 9 ) ) ;      -- ERROR: ARRAY (INTGR,LIT.) ARG.
     IA := NOT( INT_ARRAY ) ;         -- ERROR: ARRAY (INTGR, VAR.) ARG.
     BV3:= NOT (BOOL_REC_3_VAR);        -- ERROR: BOOL_REC_3 (VAR.) ARG.
     BV3:= NOT( (TRUE,TRUE,TRUE) );      -- ERROR: BOOL_REC_3(LIT.) ARG.
     BMV:= NOT( BOOL_MAT_VAR);         -- ERROR: BOOL_MATRIX (VAR.) ARG.
     BMV:= NOT(((TRUE, TRUE), (TRUE, TRUE)));     -- ERROR: BOOL_MATRIX 
                                                  --         (LIT.) ARG.

END  B45661A ;
