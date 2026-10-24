-- B24205A.ADA

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
-- CHECK THAT BASED INTEGER LITERALS MUST NOT HAVE 
-- NEGATIVE EXPONENTS (INCLUDING -0).

-- PWB  2/14/86

PROCEDURE B24205A IS

     TYPE FLOATING_1 IS DIGITS 3#2#E-1;              -- ERROR: NEG EXP.
     TYPE FLOATING_2 IS DIGITS 8#5#E-0;              -- ERROR: -0 EXP.
     TYPE SMALL_INT_1 IS RANGE -10 .. 8#10#E-1;      -- ERROR: NEG EXP.
     TYPE SMALL_INT_2 IS RANGE -10 .. 4#21#E-0;      -- ERROR: -0 EXP.
     TYPE REC_TYPE_1 (DISC : INTEGER := 5#4#E-2) IS  -- ERROR: NEG EXP.
          RECORD
               NULL;
          END RECORD;
     TYPE REC_TYPE_2 (DISC : INTEGER := 5#4#E-0) IS  -- ERROR: -0 EXP.
          RECORD
               NULL;
          END RECORD;

     TYPE ARR_TYPE IS 
          ARRAY (INTEGER RANGE <>) OF BOOLEAN;

     ARR_1 : ARR_TYPE (1..5#10#E-1);                 -- ERROR: NEG EXP.
     ARR_2 : ARR_TYPE (1..4#10#E-0);                 -- ERROR: -0 EXP.
     CON   : CONSTANT INTEGER := 5#4#E-1;            -- ERROR: NEG EXP.
     VAR   : INTEGER := 5#4#E-0;                     -- ERROR: -0 EXP.

     I1, I2, I3 : INTEGER := 0;
     C1 : CHARACTER := 'A';

BEGIN

     IF I1 > 2#101#E-1 OR                            -- ERROR: NEG EXP.
        I1 < 2#101#E-0                               -- ERROR: -0 EXP.
     THEN
          I2 := 6#53#E-2;                            -- ERROR: NEG EXP.
          I3 := 3#12#E-0;                            -- ERROR: -0 EXP.
     ELSE
          I2 := I1 + 8#5#E-2;                        -- ERROR: NEG EXP.
          I3 := I1 * 7#5#E-0;                        -- ERROR: -0 EXP.
     END IF;

     CASE I1 IS
          WHEN 6#12#E-2 =>                           -- ERROR: NEG EXP.
               C1 := CHARACTER'VAL(12#55#E-1);       -- ERROR: NEG EXP.
          WHEN 8#12#E-0 =>                           -- ERROR: -0 EXP.
               C1 := CHARACTER'VAL(13#50#E-0);       -- ERROR: -0 EXP.
          WHEN OTHERS =>
               NULL;
     END CASE;

END B24205A;
