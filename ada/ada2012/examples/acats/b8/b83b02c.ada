-- B83B02C.ADA

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
-- CHECK THAT INSIDE A LOOP THE LOOP PARAMETER HIDES ANY
--    IDENTICALLY NAMED IDENTIFIERS IN ENCOMPASSING LOOPS OR BLOCKS.


--    RM    6 JUNE 1980


PROCEDURE  B83B02C  IS

     I : INTEGER ;

     TYPE  WEEKDAY  IS  ( MON , TUE , WED , THU , FRI );

BEGIN

     FOR  LOOP_PAR  IN  1 .. 10  LOOP
          FOR  LOOP_PAR IN  MON .. FRI  LOOP
               I := 8 *
                    LOOP_PAR ; -- ERROR: ATTEMPT TO ACCESS HIDDEN
                               --       IDENTIFIER   ( 1)
          END LOOP;
     END LOOP;

     DECLARE
          LOOP_PAR : INTEGER ;
     BEGIN
          FOR  LOOP_PAR IN  MON .. FRI  LOOP
               I := 8 *
                    LOOP_PAR ; -- ERROR: ATTEMPT TO ACCESS HIDDEN
                               --       IDENTIFIER   ( 2 )

          END LOOP;
     END;


END B83B02C ;
