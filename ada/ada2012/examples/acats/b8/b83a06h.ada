-- B83A06H.ADA

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
--     CHECK THAT IF A STATEMENT LABEL IN A LOOP OF
--     A PROCEDURE  HAS THE SAME IDENTIFIER AS AN EXCEPTION
--     KNOWN OUTSIDE THE PROCEDURE, THEN NO HANDLERS FOR THAT EXCEPTION
--     CAN EXIST INSIDE THE PROCEDURE (SINCE THE INNER LABEL HIDES THE
--     EXCEPTION).

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X


-- HISTORY:
--    RM 02/16/80
--    JBG 5/16/83
--    DTN 03/31/93  REMOVED NUMERIC_ERROR FROM TEST.

PROCEDURE  B83A06H  IS

     LABEL_EXCEPTION_1  :  EXCEPTION ;
     LABEL_EXCEPTION_2  :  EXCEPTION ;
     LABEL_EXCEPTION_3  :  EXCEPTION ;

     PROCEDURE  PROC  IS
     BEGIN

          BEGIN

               << LABEL_EXCEPTION_1 >>            NULL ;
               << CONSTRAINT_ERROR  >>            NULL ;

               FOR  I  IN  INTEGER  LOOP
                    << LABEL_EXCEPTION_2 >>       NULL ;
                    << PROGRAM_ERROR  >>          NULL ;
               END LOOP;

          END ;

          FOR  I  IN  INTEGER  LOOP
               << LABEL_EXCEPTION_3 >>            NULL ;
               << STORAGE_ERROR  >>               NULL ;
          END LOOP;

     EXCEPTION

          WHEN  LABEL_EXCEPTION_1  =>  NULL ; -- OK.
          WHEN  LABEL_EXCEPTION_2  =>  NULL ; -- OK.
          WHEN  LABEL_EXCEPTION_3  =>  NULL ; -- ERROR: EXC. NAME HIDDEN
          WHEN  STORAGE_ERROR      =>  NULL ; -- ERROR: EXC. NAME HIDDEN
          WHEN  PROGRAM_ERROR      =>  NULL ; -- OK.
          WHEN  CONSTRAINT_ERROR   =>  NULL ; -- OK.

     END PROC ;

BEGIN

     NULL ;

END B83A06H ;
