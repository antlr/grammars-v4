-- B45207B.ADA

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
-- CHECK THAT EQUALITY AND INEQUALITY ARE NOT PREDEFINED FOR LIMITED
--     TYPES.


-- PART 1: LIMITED TYPES NOT INVOLVING TASKING OR TYPE DERIVATION.


-- CASES COVERED:              ( ">>" MARKS CASES COVERED IN THIS FILE.)

--    * LIMITED PRIVATE TYPE
-->>  * ARRAY WHOSE COMPONENTS ARE OF A LIMITED PRIVATE TYPE
-->>  * RECORD WITH A COMPONENT WHICH IS OF A LIMITED PRIVATE TYPE
--    * ARRAY OF LIMITED-TYPE RECORDS (AS ABOVE)
--    * RECORDS OF LIMITED-TYPE ARRAYS  (AS ABOVE)
--    * ARRAY WHOSE COMPONENTS ARE OF AN EQUALITY-ENDOWED
--          LIMITED PRIVATE TYPE
--    * RECORD ALL OF WHOSE COMPONENTS ARE OF AN EQUALITY-ENDOWED
--          LIMITED PRIVATE TYPE


-- RM  2/12/82
-- RM  2/22/82
-- JRK 2/2/84


PROCEDURE B45207B IS

BEGIN


     -------------------------------------------------------------------
     -----  ARRAY WHOSE COMPONENTS ARE OF A LIMITED PRIVATE TYPE  ------

     DECLARE

          B      : BOOLEAN := TRUE ;

          PACKAGE  P  IS
               TYPE  LP  IS  LIMITED PRIVATE;
          PRIVATE
               TYPE  LP  IS  ( AA , BB , CC );
          END  P ;

          USE  P ;

          TYPE  ARR  IS  ARRAY ( 1..3 ) OF  LP ;

          X , Y  :  ARR ;

          PACKAGE BODY  P  IS
          BEGIN

               X(1) := AA ;
               X(2) := AA ;
               X(3) := AA ;
               Y(1) := AA ;
               Y(2) := AA ;
               Y(3) := AA ;

               IF  X = Y                -- ERROR: EQUALITY NOT AVAILABLE
               THEN
                    NULL ;
               END IF;

          END  P ;

     BEGIN

          IF  X = Y                     -- ERROR: EQUALITY NOT AVAILABLE
          THEN
               NULL ;
          END IF;

          B := ( X /= Y ) ;             -- ERROR: EQUALITY NOT AVAILABLE

     END ;


     -------------------------------------------------------------------
     -----------  RECORD WITH A LIMITED-PRIVATE COMPONENT  -------------

     DECLARE

          B      : BOOLEAN := TRUE ;

          PACKAGE  P  IS
               TYPE  LP  IS  LIMITED PRIVATE;
          PRIVATE
               TYPE  LP  IS  ( AA , BB , CC );
          END  P ;

          USE  P ;

          TYPE  REC  IS
               RECORD
                    COMPONENT : LP ;
               END RECORD;

          X , Y  :  REC ; 

          PACKAGE BODY  P  IS
          BEGIN

               X.COMPONENT := AA ;
               Y.COMPONENT := AA ;

               IF  X = Y                -- ERROR: EQUALITY NOT AVAILABLE
               THEN
                    NULL ;
               END IF;

          END  P ;

     BEGIN

          IF  X = Y                     -- ERROR: EQUALITY NOT AVAILABLE
          THEN
               NULL ;
          END IF;

          B := ( X /= Y ) ;             -- ERROR: EQUALITY NOT AVAILABLE

     END ;


     -------------------------------------------------------------------


END B45207B ;
