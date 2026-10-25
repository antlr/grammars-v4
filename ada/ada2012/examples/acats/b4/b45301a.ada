-- B45301A.ADA

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
-- CHECK THAT '+' AND '-' ARE NOT PREDEFINED FOR OPERANDS HAVING 
-- DIFFERENT INTEGER TYPES.  INCLUDES A CASE IN WHICH ONE OPERAND IS
-- A NON-INTEGER DISCRETE TYPE.

-- RJW 2/8/86

PROCEDURE B45301A IS
     
BEGIN

     DECLARE
          TYPE N IS NEW INTEGER;

          I1 : INTEGER  := 0;
          I2 : N        := 0;
     BEGIN
          I1 := I1 + I2;         -- ERROR: DIFFERENT TYPES FOR '+'.
          I1 := I1 - I2;         -- ERROR: DIFFERENT TYPES FOR '-'.
     END;

     DECLARE
          TYPE N IS RANGE -100 .. 0;
          
          I1 : INTEGER := 0;
          I2 : N       := 0;
     BEGIN
          I1 := I1 + I2;         -- ERROR: DIFFERENT TYPES FOR '+'.
          I1 := I1 - I2;         -- ERROR: DIFFERENT TYPES FOR '-'.
     END;

     DECLARE
          I : INTEGER   := 0;
          C : CHARACTER := '0';     
     BEGIN
          I := I + C;          -- ERROR: DIFFERENT TYPES FOR '+'.
          I := I - C;          -- ERROR: DIFFERENT TYPES FOR '-'.
     END;

END B45301A;
