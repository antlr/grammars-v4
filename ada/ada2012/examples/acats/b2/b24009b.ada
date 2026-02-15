-- B24009B.ADA

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
--      CHECK THAT INTEGER LITERALS MUST NOT HAVE POINTS
--      (BOTH BASED AND DECIMAL).

-- HISTORY:
--      MCH  05/17/90  SPLIT FROM B24009A.ADA.

PROCEDURE B24009B IS

     TYPE FLOATING_1 IS DIGITS 5.;                   -- ERROR: 5.
     TYPE SMALL_INT_1 IS RANGE 0. .. 10;             -- ERROR: 0.
     TYPE REC_TYPE_1 (DISC : INTEGER := 5.E2) IS     -- ERROR: 5.
          RECORD
               NULL;
          END RECORD;

     TYPE ARR_TYPE IS
          ARRAY (INTEGER RANGE <>) OF BOOLEAN;

     ARR_1 : ARR_TYPE (1..10.);                      -- ERROR: 10.
     CON   : CONSTANT INTEGER := 3E4.;               -- ERROR: 4.
     VAR   : INTEGER := 3E4.0;                       -- ERROR: 4.0

     I1, I2, I3 : INTEGER := 0;
     C1 : CHARACTER := 'A';

BEGIN

     IF I1 > 2.#101# OR                              -- ERROR: 2.
        I1 < 2.0#101#                                -- ERROR: 2.0
     THEN
          I2 := 3#12.#E2;                            -- ERROR: 12.
     ELSE
          I2 := I1 + 4#10#E1.;                       -- ERROR: 1.
          I3 := I1 * 4#10#E1.0;                      -- ERROR: 1.0
     END IF;

     CASE I1 IS
          WHEN 12 =>
               C1 := CHARACTER'VAL(65.);             -- ERROR: 65.
          WHEN 10.E2 =>                              -- ERROR: 10.
               C1 := CHARACTER'VAL(66);
          WHEN OTHERS =>
               C1 := CHARACTER'VAL(8#10.#E2);        -- ERROR: 10.
     END CASE;

END B24009B;
