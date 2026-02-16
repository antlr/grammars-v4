-- B45205A.ADA

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
-- CHECK THAT RELATIONAL EXPRESSIONS OF THE FORM   A < B < C  ARE
--     FORBIDDEN.


-- RM  2/9/82
-- JBG 4/15/85

PROCEDURE B45205A IS

BEGIN

     -------------------------------------------------------------------
     -------------------------  INTEGER  -------------------------------

     DECLARE

          X , Y  : INTEGER  RANGE 0..101  :=  1 ;
          B      : BOOLEAN ;

     BEGIN

          WHILE  0 < X < 101  LOOP  -- ERROR: BAD RELATIONAL EXPRESSION.
               NULL ;
          END LOOP;

          LOOP
               EXIT WHEN  100 >= X > 0 ;  -- ERROR: BAD REL. EXPRESSION.
          END LOOP;

          IF  0 /= X /= 17          -- ERROR: BAD RELATIONAL EXPRESSION.
          THEN
               NULL ;
          END IF;

          B := ( X = Y = 1 ) ;      -- ERROR: BAD RELATIONAL EXPRESSION.

     END ;


     -------------------------------------------------------------------
     -------------------  USER-DEFINED ENUMERATIONS  -------------------

     DECLARE

          TYPE  ENUM  IS  ( AA , BB , CC , DD , EE );

          X , Y  : ENUM  RANGE AA..DD  :=  CC ;
          B      : BOOLEAN ;

     BEGIN

          WHILE  EE > X > AA  LOOP  -- ERROR: BAD RELATIONAL EXPRESSION.
               NULL ;
          END LOOP;

          LOOP
               EXIT WHEN  AA < X <= DD ;  -- ERROR: BAD REL. EXPRESSION.
          END LOOP;

          IF  BB /= X /= DD         -- ERROR: BAD RELATIONAL EXPRESSION.
          THEN
               NULL ;
          END IF;

          B := ( X = Y = CC ) ;     -- ERROR: BAD RELATIONAL EXPRESSION.

     END ;


     -------------------------------------------------------------------
     ------------------------  CHARACTER  ------------------------------

     DECLARE

          X , Y  :  CHARACTER  RANGE 'A'..'D'  :=  'C' ;
          B      :  BOOLEAN ;

     BEGIN

          WHILE  'A' <= X < 'E'  LOOP     -- ERROR: BAD REL. EXPRESSION.
               NULL ;
          END LOOP;

          LOOP
               EXIT WHEN  'D' > X >= 'A' ;-- ERROR: BAD REL. EXPRESSION.
          END LOOP;

          IF  'B' /= X /= 'D'       -- ERROR: BAD RELATIONAL EXPRESSION.
          THEN

               NULL ;
          END IF;

          B := ( X = Y = 'C' ) ;    -- ERROR: BAD RELATIONAL EXPRESSION.

     END ;

     -------------------------------------------------------------------


END B45205A;
