-- B2A005B.ADA

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
-- IN A REAL BASED LITERAL WHEN USING COLONS IN PLACE OF THE SHARP 
-- SIGNS.

-- TBN  5/22/86

PROCEDURE B2A005B IS

     TYPE FLOATING IS DIGITS 5;

     CON : CONSTANT FLOATING := 0:00.01:E+1;             -- ERROR: BASE.
     VAR : FLOATING := 17:4.1:E2;                        -- ERROR: BASE.

     I1 : INTEGER := 2:1:;
     I2, I3 : FLOATING := 2:1.1:;

BEGIN

     IF I2 > 1:00.0:E1 OR                                -- ERROR: BASE.
        I2 < 1:0.01:E0                                   -- ERROR: BASE.
     THEN
          I2 := 18:5.3:E2;                               -- ERROR: BASE.
          I3 := 32:1.2:E1;                               -- ERROR: BASE.
     ELSE
          I2 := I2 + 64:5.8:E2;                          -- ERROR: BASE.
          I3 := I2 * 17:5.1:E4;                          -- ERROR: BASE.
     END IF;

     CASE I1 IS
          WHEN 16:12:E2 =>
               I2 := I3 + 64:5.8:E2;                     -- ERROR: BASE.
          WHEN 12:12:E2 =>
               I3 := I2 + 1:7.1:E0;                      -- ERROR: BASE.
          WHEN OTHERS =>
               NULL;
     END CASE;

END B2A005B;
