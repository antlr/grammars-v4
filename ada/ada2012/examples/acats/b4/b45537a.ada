-- B45537A.ADA

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
-- CHECK THAT FIXED POINT MULTIPLICATION AND DIVISION ARE NOT 
-- PREDEFINED WHEN ONE OF THE OPERANDS IS AN INTEGER TYPE OTHER
-- THAN PREDEFINED INTEGER.

-- RJW 2/28/86

PROCEDURE B45537A IS

     TYPE FIXED IS DELTA 1.0 RANGE -1.0 .. 1.0;
     F : FIXED := 0.0;

     I : INTEGER := 1;
     
     TYPE INT1 IS NEW INTEGER;
     I1 : INT1 := 1;

     TYPE INT2 IS RANGE 1 .. 5;
     I2 : INT2 := 1;

BEGIN
     F := F * I;                    -- OK.
     F := I * F;                    -- OK.
     F := F / I;                    -- OK.

     F := F * I1;                   -- ERROR: INVALID TYPES FOR '*'.
     F := I1 * F;                   -- ERROR: INVALID TYPES FOR '*'.
     F := F / I1;                   -- ERROR: INVALID TYPES FOR '/'.

     F := F * I2;                   -- ERROR: INVALID TYPES FOR '*'.
     F := I2 * F;                   -- ERROR: INVALID TYPES FOR '*'.
     F := F / I2;                   -- ERROR: INVALID TYPES FOR '/'.
END B45537A;
