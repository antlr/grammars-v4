-- B55B18A.ADA

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
-- CHECK THAT THE LOOP_PARAMETER CANNOT BE USED IN THE DEFINITION OF THE
--     DISCRETE_RANGE.

-- RM 6/23/82
-- SPS 3/1/83
-- JBG 3/15/83

PROCEDURE B55B18A IS
BEGIN


     FOR I IN  I .. 10 LOOP   -- ERROR: SECOND USE OF  'I'  ILLEGAL.
          NULL;
     END LOOP;


     DECLARE
          J : INTEGER := 1;
     BEGIN

          FOR J IN  J .. 10 LOOP  -- ERROR: SECOND USE OF  'J'  ILLEGAL.
              NULL;
          END LOOP;

     END;

     DECLARE

          FUNCTION M RETURN INTEGER IS
          BEGIN
               RETURN 0;
          END M;

     BEGIN

L:        FOR K IN L.K .. 10 LOOP      -- ERROR: L.K.
               NULL;
          END LOOP L;

          FOR M IN M .. 10 LOOP        -- ERROR: SECOND M.
               NULL;
          END LOOP;

     END;

END B55B18A;
