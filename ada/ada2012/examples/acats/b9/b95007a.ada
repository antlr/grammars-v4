-- B95007A.ADA

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
-- CHECK THAT IF A TASK'S ENTRY IS RENAMED AS A PROCEDURE INSIDE THE
--   CORRESPONDING TASK BODY, THE PROCEDURE NAME MUST NOT BE USED IN AN
--   ACCEPT_STATEMENT.

-- JRK 10/26/81

PROCEDURE B95007A IS

     TASK T IS
          ENTRY E0;
          ENTRY E1 (I : INTEGER);
          ENTRY E2 (BOOLEAN) (I : INTEGER);
     END T;

     TASK BODY T IS

          PROCEDURE P0 RENAMES E0;
          PROCEDURE P1 (I : INTEGER) RENAMES E1;
          PROCEDURE P2 (I : INTEGER) RENAMES E2 (TRUE);

     BEGIN

          ACCEPT E0;                    -- OK.
          ACCEPT E1 (I : INTEGER);      -- OK.
          ACCEPT E2 (TRUE) (I : INTEGER); -- OK.

          ACCEPT P0;                    -- ERROR: NOT AN ENTRY.
          ACCEPT P1 (I : INTEGER);      -- ERROR: NOT AN ENTRY.
          ACCEPT P2 (I : INTEGER);      -- ERROR: NOT AN ENTRY.

          ACCEPT P0 DO                  -- ERROR: NOT AN ENTRY.
               NULL;
          END;

          ACCEPT P1 (I : INTEGER) DO    -- ERROR: NOT AN ENTRY.
               NULL;
          END;

          ACCEPT P2 (I : INTEGER) DO    -- ERROR: NOT AN ENTRY.
               NULL;
          END;

          ACCEPT P0 DO                  -- ERROR: NOT AN ENTRY.
               NULL;
          END P0;                       

          ACCEPT P1 (I : INTEGER) DO    -- ERROR: NOT AN ENTRY.
               NULL;
          END P1;                       

          ACCEPT P2 (I : INTEGER) DO    -- ERROR: NOT AN ENTRY.
               NULL;
          END P2;                       

     END T;

BEGIN
     NULL;
END B95007A;
