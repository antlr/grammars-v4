-- B57001D.ADA

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
-- CHECK THAT THE LOOP PARAMETER OF A LOOP CANNOT BE USED IN
--    AN EXIT STATEMENT INSIDE THE LOOP.

-- CHECK THAT AN EXIT STATEMENT WITH A LOOP_NAME MUST BE ENCLOSED
--    BY A LOOP STATEMENT WITH THE SAME NAME.


-- RM 03/21/81
-- SPS 3/7/83

PROCEDURE  B57001D  IS
BEGIN


     BEGIN

          FOR  LOOP_PARAMETER  IN  1..11  LOOP

               NULL ;
               EXIT  LOOP_PARAMETER ;           -- ERROR: LOOP_PARAMETER

          END LOOP;


          FOR  LOOP_PARAMETER  IN  CHARACTER  LOOP

               NULL ;

               FOR  I  IN  INTEGER RANGE 1..4  LOOP
                    NULL ;
                    EXIT  LOOP_PARAMETER  WHEN  6 = I ;-- ERROR: LOOP_P.
               END LOOP;

          END LOOP;


          LOOP_ID :
          LOOP

               NULL ;

               EXIT  LOOP_PARAMETER ;  -- ERROR: UNDECLARED (BUT USED
                                       --    ABOVE AS LOOP PARAMETER;
                                       --    NO PAR. IN THIS LOOP)

          END LOOP  LOOP_ID ;

     END ;

     -------------------------------------------------------------------

     BEGIN

          LOOP_ID1 :
          FOR  LOOP_PARAMETER  IN  1..11  LOOP

               NULL ;
               EXIT  LOOP_ID1 ;          -- OK

          END LOOP  LOOP_ID1 ;


          FOR  LOOP_PARAMETER  IN  1..11  LOOP

               NULL ;
               EXIT  LOOP_ID1 ;        -- ERROR: FOREIGN LOOP IDENTIF.

          END LOOP;


          LOOP1 :
          FOR  LOOP_PARAMETER  IN  CHARACTER  LOOP

               NULL ;

               BLOCK1 :
               BEGIN

                    LOOP2 :
                    FOR  I  IN  INTEGER RANGE 1..4  LOOP
                         NULL ;
                         EXIT  LOOP1   ;     -- OK
                         EXIT  BLOCK1  ;     -- ERROR: NOT A LOOP ID.
                         EXIT  LOOP2   ;     -- OK
                         EXIT  LOOP3   ;     -- ERROR: FOREIGN LOOP ID.
                         EXIT  LOOP_ID1;     -- ERROR: FOREIGN LOOP ID.
                    END LOOP  LOOP2 ;


                    LOOP3 :
                    FOR  I  IN  INTEGER RANGE 1..4  LOOP
                         NULL ;
                         EXIT  LOOP1   ;     -- OK
                         EXIT  BLOCK1  ;     -- ERROR: NOT A LOOP ID.
                         EXIT  LOOP2   ;     -- ERROR: FOREIGN LOOP ID.
                         EXIT  LOOP3   ;     -- OK
                         EXIT  LOOP_ID1;     -- ERROR: FOREIGN LOOP ID.
                    END LOOP  LOOP3 ;

               END  BLOCK1 ;

          END LOOP  LOOP1 ;

     END ;



END B57001D ;
