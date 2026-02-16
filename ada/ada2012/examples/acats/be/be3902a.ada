-- BE3902A.ADA

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
-- CHECK THAT ENUMERATION_IO CAN BE INSTANTIATED FOR PREDEFINED AND USER
-- DEFINED ENUMERATION TYPES.  CHECK THAT OTHER NON-DISCRETE
-- TYPES ARE NOT ACCEPTED.

-- SPS 10/7/82

WITH TEXT_IO;
USE TEXT_IO;

PROCEDURE BE3902A IS

     TYPE WEEKDAY IS (MONDAY, TUESDAY, WEDNESDAY, THURSDAY, FRIDAY);
     TYPE FLAG_COLOR IS (RED, WHITE, BLUE);
     TYPE CH IS NEW CHARACTER RANGE 'A'.. 'C';
     TYPE NB IS NEW BOOLEAN;
     TYPE INT IS NEW INTEGER;
     TYPE RC IS RECORD NULL; END RECORD;
     TYPE AC IS ACCESS CH;
     TYPE AR IS ARRAY (1.. 10) OF WEEKDAY;

     PACKAGE NE1 IS NEW ENUMERATION_IO (WEEKDAY);      -- OK.
     PACKAGE NE2 IS NEW ENUMERATION_IO (FLAG_COLOR);   -- OK.
     PACKAGE NE3 IS NEW ENUMERATION_IO (CH);           -- OK.
     PACKAGE NE4 IS NEW ENUMERATION_IO (CHARACTER);    -- OK.
     PACKAGE NE5 IS NEW ENUMERATION_IO (NB);           -- OK.
     PACKAGE NE6 IS NEW ENUMERATION_IO (BOOLEAN);      -- OK.
     PACKAGE NE7 IS NEW ENUMERATION_IO (INT);          -- OK.
     PACKAGE NE8 IS NEW ENUMERATION_IO (INTEGER);      -- OK.
     PACKAGE NE9 IS NEW ENUMERATION_IO (RC);           -- ERROR: RC.
     PACKAGE NE10 IS NEW ENUMERATION_IO (AC);          -- ERROR: AC.
     PACKAGE NE11 IS NEW ENUMERATION_IO (AR);          -- ERROR: AR.

BEGIN
     NULL;
END BE3902A;
