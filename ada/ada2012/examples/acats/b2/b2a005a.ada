-- B2A005A.ADA

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
-- CHECK THAT THE BASE CANNOT BE LESS THAN "2" OR GREATER THAN "16"
-- IN AN INTEGER BASED LITERAL WHEN USING THE COLON IN PLACE OF THE 
-- SHARP SIGN.

-- TBN  2/25/86

PROCEDURE B2A005A IS

     TYPE FLOATING_1 IS DIGITS 1:00:E+1;                 -- ERROR: BASE.
     TYPE FLOATING_2 IS DIGITS 17:5:E0;                  -- ERROR: BASE.
     TYPE SMALL_INT_1 IS RANGE -10 .. 1:10:E+1;          -- ERROR: BASE.
     TYPE SMALL_INT_2 IS RANGE -10 .. 24:21:;            -- ERROR: BASE.
     TYPE REC_TYPE_1 (DISC : INTEGER := 0:00:E2) IS      -- ERROR: BASE.
          RECORD
               NULL;
          END RECORD;
     TYPE REC_TYPE_2 (DISC : INTEGER := 25:4:E1) IS      -- ERROR: BASE.
          RECORD
               NULL;
          END RECORD;

     TYPE ARR_TYPE IS 
          ARRAY (INTEGER RANGE <>) OF BOOLEAN;

     ARR_1 : ARR_TYPE (1 .. 0:00001:E3);                 -- ERROR: BASE.
     ARR_2 : ARR_TYPE (1 .. 19:10:E0);                   -- ERROR: BASE.
     CON   : CONSTANT INTEGER := 0:0001:E+1;             -- ERROR: BASE.
     VAR   : INTEGER := 165:4:E2;                        -- ERROR: BASE.

     I1, I2, I3 : INTEGER := 0;
     C1 : CHARACTER := 'A';

BEGIN

     IF I1 > 1:000:E1 OR                                 -- ERROR: BASE.
        I1 < 1:001:E0                                    -- ERROR: BASE.
     THEN
          I2 := 18:53:E2;                                -- ERROR: BASE.
          I3 := 32:12:E1;                                -- ERROR: BASE.
     ELSE
          I2 := I1 + 64:5:E2;                            -- ERROR: BASE.
          I3 := I1 * 96:5:E4;                            -- ERROR: BASE.
     END IF;

     CASE I1 IS
          WHEN 17:12:E2 =>                               -- ERROR: BASE.
               C1 := CHARACTER'VAL(19:55:E1);            -- ERROR: BASE.
          WHEN 128:12:E2 =>                              -- ERROR: BASE.
               C1 := CHARACTER'VAL(1:50:E3);             -- ERROR: BASE.
          WHEN OTHERS =>
               NULL;
     END CASE;

END B2A005A;
