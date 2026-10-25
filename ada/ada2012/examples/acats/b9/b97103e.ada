-- B97103E.ADA

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
-- OBJECTIVE:
--     CHECK THAT A SELECTIVE_WAIT STATEMENT CANNOT APPEAR OUTSIDE A
--     TASK BODY.  (PART E: INSIDE TASK SPEC)

-- HISTORY:
--     RM  04/02/82  CREATED ORIGINAL TEST.
--     BCB 04/20/90  REMOVED UNNECESSARY CODE AND ERROR MESSAGES.

PROCEDURE  B97103E  IS
BEGIN


     -------------------------------------------------------------------


     DECLARE


          TASK TYPE  TT2  IS

               ENTRY  A ;

               SELECT                  -- ERROR: SELECTIVE_WAIT OUTSIDE
                                       --                     TASK BODY.
                         ACCEPT  A ;
               END SELECT;

          END  TT2 ;


          TASK BODY  TT2  IS
          BEGIN
               NULL ;
          END  TT2 ;


     BEGIN


          NULL ;


     END  ;

     -------------------------------------------------------------------


END  B97103E ;
