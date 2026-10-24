-- B46004B.ADA

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
-- WHEN THE TARGET TYPE OF A TYPE CONVERSION IS AN ARRAY TYPE, CHECK 
-- THAT THE OPERAND TYPE CANNOT HAVE FEWER OR MORE DIMENSIONS THAN
-- THE TARGET TYPE.

-- R.WILLIAMS 9/19/86

PROCEDURE B46004B IS

     TYPE ARR1 IS ARRAY (1 .. 2) OF INTEGER;
     A1 : ARR1 := (1 => 1, 2 => 2);

     TYPE NARR1 IS ARRAY (2 .. 1) OF INTEGER;
     NA1 : NARR1;

     TYPE ARR2 IS ARRAY (1 .. 1,  1 .. 2) OF INTEGER;
     A2 : ARR2 := (1 => (1 => 1, 2 => 2));

     TYPE NARR2 IS ARRAY (1 .. 1, 2 .. 1) OF INTEGER;
     NA2 : NARR2;

BEGIN
     A1 := ARR1 (A2);              -- ERROR: DIMENSIONALITY OF OPERAND.
     
     A2 := ARR2 (A1);              -- ERROR: DIMENSIONALITY OF OPERAND.

     A1 := ARR1 (NA2);             -- ERROR: DIMENSIONALITY OF OPERAND.

     A2 := ARR2 (NA1);             -- ERROR: DIMENSIONALITY OF OPERAND.

     NA2 := NARR2 (NA1);           -- ERROR: DIMENSIONALITY OF OPERAND.

     NA1 := NARR1 (NA2);           -- ERROR: DIMENSIONALITY OF OPERAND.
END B46004B;
