-- B95006A.ADA

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
-- CHECK THAT THE IDENTIFIER AT THE END OF AN ACCEPT_STATEMENT, IF
--   PRESENT, MUST BE THAT USED IN THE DECLARATION OF THE CORRESPONDING
--   ENTRY OR ENTRY FAMILY.

-- JRK 10/26/81
-- JWC 6/28/85   RENAMED TO -AB

PROCEDURE B95006A IS

     TASK T IS
          ENTRY E0;
          ENTRY E1 (I : INTEGER);
          ENTRY E2 (BOOLEAN) (I : INTEGER);
     END T;

     TASK BODY T IS
          J : INTEGER := 0;
     BEGIN

          ACCEPT E0 DO
               J := 1;
          END E1;                  -- ERROR: MISMATCHED IDENTIFIER.

          ACCEPT E1 (I : INTEGER) DO
               J := I;
          END E2;                  -- ERROR: MISMATCHED IDENTIFIER.

          ACCEPT E2 (TRUE) (I : INTEGER) DO
               J := I;
          END E0;                  -- ERROR: MISMATCHED IDENTIFIER.

          ACCEPT E0 DO
               ACCEPT E1 (I : INTEGER) DO
                    ACCEPT E2 (TRUE) (I : INTEGER) DO
                         J := I;
                    END E2;        -- OK.
               END E0;             -- ERROR: MISMATCHED IDENTIFIER.
          END E;                   -- ERROR: UNDEFINED IDENTIFIER.

          ACCEPT E1 (I : INTEGER) DO
               ACCEPT E2 (TRUE) (I : INTEGER) DO
                    ACCEPT E0 DO
                         J := I;
                    END E0;        -- OK.
               END E0;             -- ERROR: MISMATCHED IDENTIFIER.
          END E1;                  -- OK.

     END T;

BEGIN
     NULL;
END B95006A;
