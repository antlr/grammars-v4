-- BB3001A.ADA

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
--    CHECK THAT THE IDENTIFIER MENTIONED IN A RAISE STATEMENT
--    MUST BE AN EXCEPTION.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X


-- HISTORY:
--    DCB 04/01/80
--    JRK 11/19/80
--    VKG 01/07/83
--    JBG 4/19/83
--    DTN 03/30/93  REMOVED NUMERIC_ERROR FROM TEST.

PROCEDURE BB3001A IS

     I9 : INTEGER := 9;
     C9 : CHARACTER := 'X';
     B9 : BOOLEAN := FALSE;
     E1 : EXCEPTION;

BEGIN
     I9 := 9;

     BEGIN
          RAISE I9;     -- ERROR: RAISING A NON-EXCEPTION.
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          RAISE C9;     -- ERROR: RAISING A NON-EXCEPTION.
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          RAISE E1;     -- LEGAL RAISE STATEMENT.
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          RAISE B9;     -- ERROR: RAISING A NON-EXCEPTION.
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          RAISE CONSTRAINT_ERROR; -- LEGAL RAISE STATEMENT.
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     BEGIN
          RAISE E;      -- ERROR: E NOT DECLARED.
     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

     DECLARE

          CONSTRAINT_ERROR,
          PROGRAM_ERROR,
          STORAGE_ERROR,
          TASKING_ERROR : INTEGER := 1;

     BEGIN
          -- THE FOLLOWING STATEMENTS ATTEMPT TO RAISE EXCEPTION NAMES
          -- WHICH HAVE BEEN REDECLARED AS INTEGER.

          RAISE CONSTRAINT_ERROR;  -- ERROR: RAISING A NON-EXCEPTION.
          NULL;
          RAISE PROGRAM_ERROR;      -- ERROR: RAISING A NON-EXCEPTION.
          NULL;
          RAISE STORAGE_ERROR;     -- ERROR: RAISING A NON-EXCEPTION.
          NULL;
          RAISE TASKING_ERROR;     -- ERROR: RAISING A NON-EXCEPTION.
          NULL;

     EXCEPTION
          WHEN OTHERS =>
               NULL;
     END;

END BB3001A;
