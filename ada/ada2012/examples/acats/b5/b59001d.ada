-- B59001D.ADA

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
-- CHECK THAT JUMPS INTO COMPOUND STATEMENTS ARE NOT ALLOWED.

-- RM  06/09/81
-- SPS 03/08/83
-- EG  10/18/85  CORRECT ERROR COMMENTS

PROCEDURE  B59001D  IS
BEGIN

     BEGIN

          GOTO  L111 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          IF  FALSE  THEN
               << L111 >>
               NULL ;
          ELSE
               << L112 >>
               NULL ;
          END IF;

          GOTO  L112 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          GOTO  L131 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          IF  FALSE  THEN
               << L133 >>
               NULL ;
          ELSE
               FOR  J  IN  1..1  LOOP
                    << L131 >>
                    << L132 >>
                    NULL ;
               END LOOP;
          END IF;

          GOTO  L132 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT
          GOTO  L133 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          GOTO  L211 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          CASE  2  IS
               WHEN  1  =>
                    << L211 >>
                    NULL ;
               WHEN  OTHERS  =>
                    << L212 >>
                    NULL ;
          END CASE;

          GOTO  L212 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          GOTO  L231 ;            -- ERROR: UNDCL LABEL

          CASE  2  IS
               WHEN  1  =>
                    << L233 >>
                    NULL ;
               WHEN  OTHERS  =>
                    DECLARE
                    BEGIN
                         << L231 >>
                         << L232 >>
                         NULL ;
                    END ;
          END CASE;

          GOTO  L232 ;            -- ERROR: UNDCL LABEL
          GOTO  L233 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          GOTO  L311 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          FOR  I  IN  1..1  LOOP
               << L311 >>
               << L312 >>
               NULL ;
          END LOOP;

          GOTO  L312 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          GOTO  L331 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          FOR  I  IN  1..1  LOOP
               CASE  2  IS
                    WHEN  1  =>
                         << L331 >>
                         NULL ;
                    WHEN  OTHERS  =>
                         << L332 >>
                         NULL ;
               END CASE;
          END LOOP;

          GOTO  L332 ;            -- ERROR: JUMP INTO COMPOUND STATEMEMT

          GOTO  L411 ;            -- ERROR: UNDCL LABEL

          DECLARE
               K : INTEGER := 17 ;
          BEGIN
               << L411 >>
               NULL ;
               << L412 >>
               NULL ;
          END;

          GOTO  L412 ;            -- ERROR: UNDCL LABEL

     END ;

END B59001D;
