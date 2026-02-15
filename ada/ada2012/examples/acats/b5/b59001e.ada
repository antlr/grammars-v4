-- B59001E.ADA

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
--     CHECK THAT JUMPS BETWEEN BRANCHES OF A 'CASE' STATEMENT  OR OF AN
--     'IF' STATEMENT  ARE NOT PERMITTED.

-- HISTORY:
--     RM  06/04/81
--     SPS 03/08/83
--     SPS 09/02/83
--     RDH 04/17/90  MODIFIED SO THAT EACH LABEL IS ONLY USED ONCE.

PROCEDURE  B59001E  IS
BEGIN

     DECLARE

          I : INTEGER := 17 ;

     BEGIN

          CASE  I  IS
               WHEN  17  =>
                    << L1 >>
                    << L2 >>
                    GOTO L3 ;              -- ERROR: TO THE OTHER BRANCH
               WHEN  OTHERS  =>
                    << L3 >>
                    GOTO L1 ;              -- ERROR: TO THE OTHER BRANCH

                    DECLARE
                    BEGIN
                         GOTO  L2 ;        -- ERROR: TO THE OTHER BRANCH
                    END ;
          END CASE;

          NULL ;

          IF  I = 17  THEN
               << L4 >>
               << L5 >>
               << L6 >>
               << L7 >>
               << L8 >>
               GOTO L9 ;                   -- ERROR: TO ANOTHER BRANCH
               GOTO L14 ;                  -- ERROR: TO ANOTHER BRANCH
          ELSIF  I = 19  THEN
               << L9 >>
               << L10 >>
               << L11 >>
               << L12 >>
               << L13 >>
               GOTO L4 ;                   -- ERROR: TO ANOTHER BRANCH
               GOTO L15 ;                  -- ERROR: TO ANOTHER BRANCH

               DECLARE
               BEGIN
                    GOTO  L5 ;             -- ERROR: TO ANOTHER BRANCH
                    GOTO  L16 ;            -- ERROR: TO ANOTHER BRANCH
               END ;
          ELSE
               << L14 >>
               << L15 >>
               << L16 >>
               GOTO L10 ;                  -- ERROR: TO ANOTHER BRANCH
               GOTO L6 ;                   -- ERROR: TO ANOTHER BRANCH
               CASE  I  IS
                    WHEN  17  =>
                         << L17 >>
                         GOTO L11 ;        -- ERROR: TO ANOTHER BRANCH
                         GOTO L7 ;         -- ERROR: TO ANOTHER BRANCH
                         GOTO L18 ;        -- ERROR: TO ANOTHER BRANCH
                    WHEN  OTHERS  =>
                         << L18 >>
                         GOTO L12 ;        -- ERROR: TO ANOTHER BRANCH
                         GOTO L8 ;         -- ERROR: TO ANOTHER BRANCH
                         GOTO L17 ;        -- ERROR: TO ANOTHER BRANCH

                         DECLARE
                         BEGIN
                              GOTO  L13 ;  -- ERROR: TO ANOTHER BRANCH
                         END ;
               END CASE;

          END IF;


     END ;


END B59001E;
