-- BD2A85B.ADA

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
--     CHECK THAT A SIZE SPECIFICATION FOR AN ACCESS TYPE MUST BE
--     REJECTED IF THE SIZE SPECIFICATION IS TOO SMALL (WHERE A
--     COLLECTION SIZE SPECIFICATION HAS BEEN GIVEN).

-- HISTORY:
--     DHH 08/22/88 CREATED ORIGINAL TEST.

WITH SYSTEM; USE SYSTEM;
PROCEDURE BD2A85B IS

     UNITS_PER_INTEGER : CONSTANT := (SYSTEM.STORAGE_UNIT - 1)/
                                      SYSTEM.STORAGE_UNIT;
     TYPE ACCESS_TYPE IS ACCESS INTEGER;
     FOR ACCESS_TYPE'STORAGE_SIZE USE 36 * UNITS_PER_INTEGER;

     FOR ACCESS_TYPE'SIZE USE 3;                    -- ERROR: TOO SMALL.

BEGIN
     NULL;
END BD2A85B;
