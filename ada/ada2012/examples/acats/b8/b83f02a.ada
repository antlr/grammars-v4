-- B83F02A.ADA

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
-- CHECK THAT AN IDENTIFIER DECLARED IN THE PACKAGE SPECIFICATION
--    CANNOT BE REDECLARED IN THE PACKAGE BODY.

--    RM    15 AUGUST 1980


PROCEDURE  B83F02A  IS

     PACKAGE  P  IS

          X1 : BOOLEAN := FALSE ;
          X2 : INTEGER RANGE 1..23 := 11 ;
          Y1 , Y3 : BOOLEAN := TRUE ;
          Y2 , Y4 : INTEGER := 5 ;
          T1 : INTEGER ;
          Z  : INTEGER ;
          TYPE  T2  IS  ( U , V , W );
          X3 , X4 : T2 ;
          SUBTYPE  T3  IS  T2 RANGE U..V ;
          SUBTYPE  T4  IS  T3 RANGE U..U ;
          X5 , X6  : T3 ;
          X7 , X8  : T4 ;
          SUBTYPE  T5  IS INTEGER RANGE 3..7 ;
          SUBTYPE  T6  IS INTEGER RANGE 3..7 ;
          
          PACKAGE  P2  IS
               I : INTEGER ;
          END P2 ;

          PROCEDURE  PROC3 ;

     END  P ;


     PACKAGE BODY  P  IS

          -- 'ERROR 1'  -  SAME TYPE, SUBTYPE
          -- 'ERROR 2'  -  (TYPED VARIABLES) NOT OF SAME TYPE & SUBTYPE
          -- 'ERROR 3'  -  NOT TYPED VARIABLES
          Y1 : INTEGER ;                            -- ERROR: 2
          Y2 : INTEGER ;                            -- ERROR: 1
          Y3 : BOOLEAN := TRUE ;                    -- ERROR: 1
          Y4 : INTEGER RANGE 5..5 ;                 -- ERROR: 2
          X1 : INTEGER RANGE 1..23 := 17 ;          -- ERROR: 2
          X2 : INTEGER RANGE 1..23 := 17 ;          -- ERROR: 1
          TYPE  T1  IS  ( A , B , C) ;              -- ERROR: 3
          Z : T2 ;                                  -- ERROR: 3
          X3 :  T2 ;                                -- ERROR: 1 , COM-
          --   POUNDED BY T2'S REDECLARATION BELOW ( NON-IDENTICAL;
          --   ENUMERATION-TO-INTEGER)
          X4 : T3 ;                                 -- ERROR: 2 (DIF-
          --    FERENT CONSTRAINTS)
          X5 : T3 ;                                 -- ERROR: 1
          X6 :  T2 ;                                -- ERROR: 2 , COM-
          --   POUNDED BY T2'S REDECLARATION BELOW ( NON-IDENTICAL;
          --   ENUMERATION-TO-INTEGER)
          SUBTYPE  T2  IS  INTEGER RANGE 1..2 ;     -- ERROR: 3 ("2")
          SUBTYPE  T4  IS  T3 RANGE U..U ;          -- ERROR: 3 ("1")
          X8 : T4 ;                                 -- ERROR: 1 ,
          --    COMPOUNDED BY THE PREVIOUS OCCURRENCE OF TWO
          --    (IDENTICAL) DECLARATIONS OF  T4
          T5 : INTEGER RANGE 3..7 ;                 -- ERROR: 3
          SUBTYPE  T6  IS  INTEGER RANGE 3..4 ;     -- ERROR: 3
          P2 : INTEGER ;                            -- ERROR: 3
          PROC3 : INTEGER ;                         -- ERROR: 3
          --    PROCEDURE-TO-INTEGER; PROCEDURE'S BODY NOT GIVEN

          PROCEDURE PROC3 IS
          BEGIN
               NULL;
          END PROC3;

     END P ;


BEGIN

     NULL ;

END B83F02A;
