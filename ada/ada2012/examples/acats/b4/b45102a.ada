-- B45102A.ADA

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
-- CHECK THAT NON-BOOLEAN ARGUMENTS TO  'AND' , 'OR' , 'XOR'
--    ARE FORBIDDEN.


-- RM   30 SEPTEMBER 1980


PROCEDURE  B45102A  IS

     TYPE  BOOL_REC_3  IS
          RECORD
               P , Q , R : BOOLEAN ;
          END RECORD ;

     TYPE  BOOL_REC_1  IS
          RECORD
               S : BOOLEAN ;
          END RECORD ;

     INTEGER_VAR      :  INTEGER   := 11 ;
     CHARACTER_VAR    :  CHARACTER := 'A' ;
     INT_ARRAY ,   IA :  ARRAY( 1..3 ) OF INTEGER := ( 1 , 2 , 3 ) ;
     CHAR_STRING , CS : STRING( 1..4 ) := "ABCD" ;
     BOOL_REC_3_VAR   :  BOOL_REC_3 := ( TRUE , TRUE , TRUE ) ;
     BOOL_REC_1_VAR   :  BOOL_REC_1 := ( S => TRUE ) ;
 

BEGIN

-- 192 CASES ( 4 * 2  TYPES ,
--               3    OPERATORS ,
--               2    ARGUMENT POSITIONS ,
--               2    TYPE OF RESULT IS BOOLEAN OR SAME AS THE OPERANDS'
--               2 :  LITERAL/VARIABLE )   RANDOMIZED INTO 12,
--    ACCORDING TO THE FOLL. SCHEME:
--
--                                 T1      T2      T3      T4 
--
--                        AND    V1L1;B  V2L2;S  V1L1;B  V2L2;S
--                        OR     L2V2;B  L1V1;S  L2V2;B  L1V1;S
--                        XOR    V1L1;B  L1V1;S  L2V2;B  V2L2;S
--
--    (THE VARIABLES WERE DECLARED IN THE ORDER
--                     T11-T12-T21-T22-T31-T32-T41-T42 ).


     IF  INTEGER_VAR AND 1     -- ERROR: INTEGER (VAR.) ARG. TO 'AND' 
                               -- ALSO INTEGER (LIT.) ARG. TO 'AND'
     THEN  NULL ;
     END IF;

     CS := CHAR_STRING AND "EFGH";  -- ERROR: ARRAY (CHAR, VAR.) ARG. TO
                                    -- 'AND' 
                                    -- ALSO ARRAY (CHAR, LIT.) ARG. 

     IF  BOOL_REC_3_VAR AND (TRUE,TRUE,TRUE)  -- ERROR: BOOL_REC_3
                                              -- (VAR.) ARG. TO 'AND' 
                              -- ALSO BOOL_REC_3 (LIT.) ARG. TO 'AND'
     THEN  NULL ;
     END IF;

     IF  'W' OR CHARACTER_VAR      -- ERROR: CHARACTER (LIT.) ARG. 
                                   -- ALSO CHARACTER (VAR.) ARG. 
     THEN  NULL ;
     END IF;

     IA := ( 7 , 8 , 9 ) OR INT_ARRAY; -- ERROR: ARRAY (INTGR, LIT.)
                                       -- ARG. TO 'OR' 
                                       -- ALSO ARRAY (INTGR, VAR.) ARG.

     IF  ( S => TRUE) OR BOOL_REC_1_VAR     -- ERROR: BOOL_REC_1 (LIT.)
                                            -- ARG. TO 'OR' 
                              -- ALSO BOOL_REC_1 (VAR.) ARG. TO 'OR'
     THEN  NULL ;
     END IF;

     IF  INTEGER_VAR XOR 1    -- ERROR: INTEGER (VAR.) ARG. TO 'XOR' 
                              -- ALSO INTEGER (LIT.) ARG. TO 'XOR'
     THEN  NULL ;
     END IF;

     IA := ( 7 , 8 , 9 ) XOR INT_ARRAY;     -- ERROR: ARRAY (INTG, LIT.)
                                            -- ARG. TO 'XOR' 
                                            -- ALSO ARRAY (INTG, VAR.) 

     IF  ( S => TRUE) XOR BOOL_REC_1_VAR    -- ERROR: BOOL_REC_1 (LIT.)
                                            -- ARG. TO 'XOR' 
                                       -- ALSO BOOL_REC_1 (VAR.) ARG. 
     THEN  NULL ;
     END IF;



END  B45102A ;
