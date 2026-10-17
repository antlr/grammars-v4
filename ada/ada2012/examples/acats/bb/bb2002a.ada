-- BB2002A.ADA

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
--    CHECK THAT AN EXCEPTION CANNOT BE REFERRED TO MORE THAN ONCE
--    IN A SINGLE HANDLER OR A SEQUENCE OF HANDLERS.

-- *** NOTE: This test has been modified since ACVC version 1.11 to    -- 9X
-- ***       remove incompatibilities associated with the transition   -- 9X
-- ***       to Ada 9X.                                                -- 9X


-- HISTORY:
--    DCB 05/07/80
--    JRK 11/17/80
--    SPS 3/23/83
--    DTN 03/30/93  REMOVED NUMERIC_ERROR FROM TEST.
--    PWN 01/31/95  REMOVED INCONSISTENCIES WITH ADA 9X.

PROCEDURE BB2002A IS

     I1 : INTEGER;

     PROCEDURE P IS
          SINGULAR : EXCEPTION;
          PLURAL   : EXCEPTION;
          I1 : INTEGER;
     BEGIN
          I1 := 1;
     EXCEPTION
          WHEN SINGULAR =>
               NULL;
          WHEN P.SINGULAR =>         -- ERROR: 2ND HANDLER FOR SINGULAR.
               NULL;
          WHEN SINGULAR   =>         -- ERROR: 3RD HANDLER FOR SINGULAR.
               NULL;
     END P;

BEGIN
     I1 := 2;
EXCEPTION
     WHEN CONSTRAINT_ERROR =>
          NULL;
     WHEN STORAGE_ERROR =>
          NULL;
     WHEN CONSTRAINT_ERROR =>        -- ERROR: 2ND HANDLER FOR
                                     -- CONSTRAINT_ERROR.
          NULL;
     WHEN STANDARD.STORAGE_ERROR =>  -- ERROR: 2ND HANDLER FOR
                                     -- STORAGE_ERROR.
          NULL;
END BB2002A;
