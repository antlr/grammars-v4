-- B45208C.ADA

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
-- CHECK THAT THE ORDERING OERATORS ARE NOT PREDEFINED FOR LIMITED
--     TYPES, MULTIDIMENSIONAL ARRAY TYPES, RECORD TYPES, ACCESS
--     TYPES, AND FOR TYPES DERIVED FROM THESE.


-- PART 1:  TYPES NOT INVOLVING TASKING OR TYPE DERIVATION.


-- CASES COVERED:              ( ">>" MARKS CASES COVERED IN THIS FILE.)

--    * LIMITED PRIVATE TYPE
--    * ARRAY WHOSE COMPONENTS ARE OF A LIMITED PRIVATE TYPE
--          (ONE-DIMENSIONAL ARRAYS ONLY; SUCCESSFUL HANDLING
--          OF MULTIDIMENSIONAL ARRAYS IS IMPLIED BY THAT OF
--          MULTIDIMENSIONAL ARRAYS OF DISCRETE TYPES.)
--   ** ACCESS TO A LIMITED PRIVATE TYPE
--          (NOT DONE; SEE BELOW.)
--   ** RECORD WITH A COMPONENT WHICH IS OF A LIMITED PRIVATE TYPE
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
-->>  * ACCESS TO A DISCRETE TYPE
--          (FOR SIMILAR CASES, SEE BELOW.)
-->>  * RECORD WITH A COMPONENT WHICH IS OF A DISCRETE TYPE
--          (FOR SIMILAR CASES, SEE BELOW.)
--   ** MULTIDIMENSIONAL ARRAY OF DISCRETE-TYPE COMPONENTS
--          (DONE UNDER 4.5.2.F/T61)
--          (FOR SIMILAR CASES, SEE BELOW.)
--   ** ARRAY OF RECORDS
--          (NOT DONE; SEE BELOW.)
--   ** RECORDS OF ARRAYS
--          (NOT DONE;  SUCCESSFUL HANDLING OF THESE (AND OF MORE
--          HIGHLY COMPOSITE) TYPES IS IMPLIED BY THE SUCCESSFUL
--          HANDLING OF
--             * ACCESS TO DISCRETE-TYPE OBJECTS;
--             * ONE-DIMENSIONAL ARRAYS;
--             * MULTIDIMENSIONAL ARRAYS OF DISCRETE-TYPE COMPONENTS;
--             * RECORDS CONTAINING DISCRETE-TYPE COMPONENTS. 
--          )

 
-- RM  2/24/82
-- SPS 12/10/82

PROCEDURE  B45208C  IS

BEGIN

     -------------------------------------------------------------------
     ------------------  ACCESS TO A DISCRETE TYPE  --------------------

     DECLARE

          B      : BOOLEAN := TRUE ;

          TYPE  DISCR  IS  ( AA , BB , CC );

          TYPE  ACC  IS ACCESS  DISCR ;

          X  :  ACC := NEW  DISCR '( AA );
          Y  :  ACC := NEW  DISCR '( AA );

     BEGIN

          IF  X > Y                     -- ERROR: ORDERING NOT AVAILABLE
          THEN
               NULL ;
          END IF;

          B := ( X <= Y ) ;             -- ERROR: ORDERING NOT AVAILABLE

     END ;


     -------------------------------------------------------------------
     ------------  RECORD WITH A DISCRETE-TYPE COMPONENT  --------------

     DECLARE

          B      : BOOLEAN := TRUE ;

          TYPE  DISCR  IS  ( AA , BB , CC );

          TYPE  REC  IS
               RECORD
                    COMPONENT : DISCR := AA ;
               END RECORD;

          X , Y  :  REC ;


     BEGIN

          IF  X > Y                     -- ERROR: ORDERING NOT AVAILABLE
          THEN
               NULL ;
          END IF;

          B := ( X <= Y ) ;             -- ERROR: ORDERING NOT AVAILABLE

     END ;


     -------------------------------------------------------------------


END B45208C ;
