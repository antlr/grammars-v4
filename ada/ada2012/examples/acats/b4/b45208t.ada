-- B45208T.ADA 

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
-- CHECK THAT THE ORDERING OPERATORS ARE NOT PREDEFINED FOR LIMITED
--     TYPES, MULTIDIMENSIONAL ARRAY TYPES, RECORD TYPES, ACCESS
--     TYPES, AND FOR TYPES DERIVED FROM THESE.

 
-- PART 4: LIMITED TYPES INVOLVING BOTH TASKING AND TYPE DERIVATION.


-- CASES COVERED (ALL TYPES COVERED HERE INVOLVE TASK TYPES WHOSE
--     TASK NATURE IS DISCLOSED UP FRONT, AS DISTINGUISHED FROM
--     LIMITED PRIVATE TYPES WHICH TURN OUT TO BE REALIZED AS
--     (OR IN TERMS OF) TASKS;  F U R T H E R M O R E ,
--     ALL TYPES COVERED HERE INVOLVE DERIVATION, EITHER
--     "OUTSIDE" (E.G. TYPE DERIVED FROM AN ARRAY OF TASKS),
--     OR "INSIDE" (E.G. ARRAY OF OBJECTS WHOSE TYPE IS DERIVED FROM
--     A TASK TYPE) :       ( ">>" MARKS CASES COVERED IN THIS FILE.)

--    * TASK TYPE
-->>  * ARRAY WHOSE COMPONENTS ARE OF A TASK TYPE
--          (ONE-DIMENSIONAL ARRAYS ONLY; SUCCESSFUL HANDLING
--          OF MULTIDIMENSIONAL ARRAYS IS IMPLIED BY THAT OF
--          MULTIDIMENSIONAL ARRAYS OF DISCRETE TYPES.)
--   ** ACCESS TO A TASK-TYPE OBJECT
--          (NOT DONE; SEE BELOW.)
--   ** RECORD WITH A COMPONENT WHICH IS OF A TASK TYPE
--          (NOT DONE; SEE BELOW.)
--   ** ARRAY OF LIMITED-TYPE RECORDS (AS ABOVE)
--          (NOT DONE; SEE BELOW.)
--   ** RECORDS OF LIMITED-TYPE ARRAYS  (AS ABOVE)
--          (NOT DONE;  SUCCESSFUL HANDLING OF THESE (AND OF MORE
--          HIGHLY COMPOSITE) LIMITED TYPES IS IMPLIED BY THE SUCCESSFUL
--          HANDLING OF
--             * ACCESS TO DISCRETE-TYPE OBJECTS;
--             * ONE-DIMENSIONAL ARRAYS;
--             * MULTIDIMENSIONAL ARRAYS OF DISCRETE-TYPE COMPONENTS;
--             * RECORDS CONTAINING DISCRETE-TYPE COMPONENTS. 
--          )

 
-- RM  2/25/82


PROCEDURE  B45208T  IS

BEGIN

     -------------------------------------------------------------------
     ---------  D(ARRAY WHOSE COMPONENTS ARE OF A TASK TYPE)  ----------
     
     DECLARE

          B      : BOOLEAN := TRUE ;

          TASK TYPE  TT  IS
               ENTRY  E1 ;
          END  TT ;

          TYPE  ARR  IS ARRAY ( 1..3 ) OF  TT ;
          TYPE  DARR  IS NEW  ARR ;

          X , Y  :  DARR ;

          TASK BODY  TT  IS
          BEGIN
               ACCEPT  E1  DO
                    NULL;
               END  E1 ;
          END  TT ;


     BEGIN

          IF  X > Y                     -- ERROR: ORDERING NOT AVAILABLE
          THEN
               NULL ;
          END IF;

          B := ( X >= Y ) ;             -- ERROR: ORDERING NOT AVAILABLE

     END ;


     -------------------------------------------------------------------
     ---------  ARRAY WHOSE COMPONENTS ARE OF A D(TASK) TYPE  ----------
     
     DECLARE

          B      : BOOLEAN := TRUE ;

          TASK TYPE  TT  IS
               ENTRY  E1 ;
          END  TT ;

          TYPE  DTT  IS NEW  TT ;
          TYPE  ARR  IS ARRAY ( 1..3 ) OF  DTT ;

          X , Y  :  ARR ;

          TASK BODY  TT  IS
          BEGIN
               ACCEPT  E1  DO
                    NULL;
               END  E1 ;
          END  TT ;


     BEGIN

          IF  X > Y                     -- ERROR: ORDERING NOT AVAILABLE
          THEN
               NULL ;
          END IF;

          B := ( X >= Y ) ;             -- ERROR: ORDERING NOT AVAILABLE

     END ;


     -------------------------------------------------------------------


END B45208T ;
