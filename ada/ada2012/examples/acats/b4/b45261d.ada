-- B45261D.ADA

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
-- CHECK THAT THE ORDERING OPERATORS ARE NOT PREDEFINED FOR MULTI-
--     DIMENSIONAL ARRAYS OF FOR ONE-DIMENSIONAL ARRAYS OF NON-DISCRETE
--     TYPES.

-- CASES COVERED:              ( ">>" MARKS CASES COVERED IN THIS FILE.)

--    * ARRAY ( INT , INT , INT )
--    * ARRAY ( CHAR , CHAR )
--    * ARRAY ( BOOL , USER-DEF. ENUM. )

--    * ARRAY ( ACCESS-TO-INT )
--    * ARRAY ( "<"-ENDOWED ACCESS-TO-INT )
--    * ARRAY ( CHARACTERSTRING )
--    * ARRAY ( FIXED )
-->>  * ARRAY ( FLOAT )

 
-- RM  2/26/82


PROCEDURE B45261D IS

BEGIN

     -------------------------------------------------------------------
     --------------------  ARRAY ( FLOAT )  ----------------------------

     DECLARE

          B      : BOOLEAN := TRUE ;

          TYPE  ARR  IS ARRAY (1..3) OF FLOAT ;

          X , Y  :  ARR  :=  ( 1..3  => 125.125 );

     BEGIN

          IF  X > Y                     -- ERROR: ORDERING NOT AVAILABLE
          THEN
               NULL ;
          END IF;

          B := ( X <= Y ) ;             -- ERROR: ORDERING NOT AVAILABLE

     END ;


     -------------------------------------------------------------------


END B45261D ;
