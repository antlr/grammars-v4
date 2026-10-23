-- B57001A.ADA

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
-- CHECK THAT EXIT STATEMENTS CANNOT BE WRITTEN OUTSIDE A LOOP BODY.


-- RM 03/16/81
-- SPS 3/7/83

PROCEDURE  B57001A  IS
BEGIN


     BEGIN

          FOR  I  IN  1..11  LOOP
               EXIT ;                        -- OK
          END LOOP;

          EXIT ;                             -- ERROR: OUTSIDE LOOP BODY

     END ;


     BEGIN

          FOR  I  IN  CHARACTER  LOOP
               EXIT ;                        -- OK
          END LOOP;

          EXIT ;                             -- ERROR: OUTSIDE LOOP BODY

     END ;


     BEGIN

          FOR  I  IN  1..11  LOOP
               EXIT WHEN I>22 ;              -- OK
          END LOOP;

          EXIT WHEN 7>3 ;                    -- ERROR: OUTSIDE LOOP BODY

     END ;


     BEGIN

          LOOP_ID :
          FOR  I  IN  1..11  LOOP
               EXIT LOOP_ID ;                -- OK
          END LOOP  LOOP_ID ;

          EXIT LOOP_ID ;                     -- ERROR: OUTSIDE LOOP BODY

     END ;


     BLOCK_ID :
     BEGIN

          FOR  I  IN  1..11  LOOP
               EXIT WHEN  I = 11 ;           -- OK
          END LOOP;

          EXIT BLOCK_ID ;                    -- ERROR: OUTSIDE LOOP BODY

     END  BLOCK_ID ;


END B57001A ;
