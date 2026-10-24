-- B44002B.ADA

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
-- CHECK THAT THE NAMES OF TASKS AND ENTRIES
--    ARE NOT PERMITTED AS PRIMARIES.


-- RM   22 SEPTEMBER 1980
-- VKG  07 JANUARY   1983

PROCEDURE  B44002B  IS

     A    : INTEGER ;

     TASK  BRINGER  IS
          ENTRY  READ ( X : OUT INTEGER );
     END ;

     TASK TYPE  READER  IS
          ENTRY  READ ( X : OUT INTEGER );
     END ;

     READER_1  :  READER ;

     TASK BODY  BRINGER  IS
     BEGIN
          ACCEPT  READ ( X : OUT INTEGER )  DO
               X :=  17 ;
          END READ ;
     END BRINGER ;

     TASK BODY  READER  IS
     BEGIN
          ACCEPT  READ ( X : OUT INTEGER )  DO
               X :=  17 ;
          END READ ;
     END READER ;

BEGIN

     A := BRINGER ;             -- ERROR: TASK AS PRIMARY
     NULL ;
     A := READER ;              -- ERROR: TASK TYPE AS PRIMARY
     NULL ;
     A := READER_1 ;            -- ERROR: TASK AS PRIMARY
     NULL ;

     A := BRINGER.READ( A );    -- ERROR: TASK ENTRY AS PRIMARY
     NULL ;
     A := READER_1.READ( A );   -- ERROR: TASK ENTRY AS PRIMARY
     NULL ;

END  B44002B ;
