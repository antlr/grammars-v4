-- B45207S.ADA

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


-- PART 4: LIMITED TYPES INVOLVING BOTH TASKING AND TYPE DERIVATION.


-- CASES COVERED (ALL TYPES COVERED HERE INVOLVE TASK TYPES WHOSE
--     TASK NATURE IS DISCLOSED UP FRONT, AS DISTINGUISHED FROM
--     LIMITED PRIVATE TYPES WHICH TURN OUT TO BE REALIZED AS
--     (OR IN TERMS OF) TASKS;  F U R T H E R M O R E ,
--     ALL TYPES COVERED HERE INVOLVE DERIVATION, EITHER
--     "OUTSIDE" (E.G. TYPE DERIVED FROM AN ARRAY OF TASKS),
--     OR "INSIDE" (E.G. ARRAY OF OBJECTS WHOSE TYPE IS DERIVED FROM
--     A TASK TYPE) :       ( ">>" MARKS CASES COVERED IN THIS FILE.)

-->>  * TASK TYPE
--    * ARRAY WHOSE COMPONENTS ARE OF A TASK TYPE
--    * RECORD WITH A COMPONENT WHICH IS OF A TASK TYPE
--    * ARRAY OF LIMITED-TYPE RECORDS (AS ABOVE)
--    * RECORDS OF LIMITED-TYPE ARRAYS  (AS ABOVE)
--    * ARRAY WHOSE COMPONENTS ARE OF AN EQUALITY-ENDOWED
--          TASK TYPE
--    * RECORD ALL OF WHOSE COMPONENTS ARE OF AN EQUALITY-ENDOWED
--          TASK TYPE


-- RM  2/23/82


PROCEDURE B45207S IS

BEGIN

     -------------------------------------------------------------------
     --------------------------  D(TASK)  ------------------------------

     DECLARE

          B      : BOOLEAN := TRUE ;

          TASK TYPE  TT  IS
               ENTRY  E1 ;
          END  TT ;

          TYPE  DTT  IS NEW  TT ;
          X , Y  :  DTT ;

          TASK BODY  TT  IS
          BEGIN
               ACCEPT  E1  DO
                    NULL;
               END  E1 ;
          END  TT ;

     BEGIN

          IF  X = Y                     -- ERROR: EQUALITY NOT AVAILABLE
          THEN
               NULL ;
          END IF;

          B := ( X /= Y ) ;             -- ERROR: EQUALITY NOT AVAILABLE

     END ;


     -------------------------------------------------------------------


END B45207S ;
