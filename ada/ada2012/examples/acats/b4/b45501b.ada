-- B45501B.ADA

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
--     THE OPERATORS * AND / ARE NOT PREDEFINED WHEN THE OPERANDS ARE
--     OF AN INTEGER AND A FLOATING POINT TYPE, OR WHEN ONE OR BOTH
--     OPERANDS ARE ARRAYS OF INTEGERS.

-- HISTORY:
--     BCB 07/14/88  CREATED ORIGINAL TEST.

PROCEDURE B45501B IS

     TYPE ARR IS ARRAY(1..5) OF INTEGER;

     A : FLOAT := 1.0;

     B, C : INTEGER := 1;

     D, E, F : ARR := (1,2,3,4,5);

BEGIN

     C := A / B;                  -- ERROR: FLOAT DIVIDED BY INTEGER.
     C := B / A;                  -- ERROR: INTEGER DIVIDED BY FLOAT.
     C := A * B;                  -- ERROR: FLOAT MULTIPLIED BY INTEGER.
     C := B * A;                  -- ERROR: INTEGER MULTIPLIED BY FLOAT.

     C := B / D;                  -- ERROR: INTEGER DIVIDED BY ARRAY.
     C := D / B;                  -- ERROR: ARRAY DIVIDED BY INTEGER.
     C := B * D;                  -- ERROR: INTEGER MULTIPLIED BY ARRAY.
     C := D * B;                  -- ERROR: ARRAY MULTIPLIED BY INTEGER.

     F := D / E;                  -- ERROR: ARRAY DIVIDED BY ARRAY.
     F := D * E;                  -- ERROR: ARRAY MULTIPLIED BY ARRAY.

END B45501B;
