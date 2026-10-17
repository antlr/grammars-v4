-- B46004D.ADA

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
-- THAT THE COMPONENT BASE TYPE OF AN ARRAY OPERAND MUST BE THE SAME AS
-- THE COMPONENT BASE TYPE OF THE TARGET TYPE.

-- R.WILLIAMS 9/19/86

PROCEDURE B46004D IS

     TYPE NEWINT IS NEW INTEGER;
     TYPE FIXED IS DELTA 1.0 RANGE -100.0 .. 100.0;
     TYPE ENUM IS (A, B, C, D, E);
     TYPE NEWE1 IS NEW ENUM;
     TYPE NEWE2 IS NEW ENUM;
          
     TYPE ARR1 IS ARRAY (INTEGER RANGE <>) OF INTEGER;
     A1 : ARR1 (1 .. 5) := (OTHERS => 0);
     N1 : ARR1 (5 .. 1);

     TYPE ARR2 IS ARRAY (1 .. 5) OF NEWINT;
     A2 : ARR2 := (OTHERS => 0);

     TYPE ARR3 IS ARRAY (INTEGER RANGE <>) OF FIXED;
     A3 : ARR3 (1 .. 5) := (OTHERS => 0.0);
     N3 : ARR3 (5 .. 1);

     TYPE ARR4 IS ARRAY (INTEGER RANGE <>) OF ENUM;
     A4 : ARR4 (1 .. 5) := (OTHERS => A);

     TYPE ARR5 IS ARRAY (1 .. 5) OF NEWE1;
     A5 : ARR5 := (OTHERS => A);

     TYPE ARR6 IS ARRAY (1 .. 5) OF NEWE2;
     A6 : ARR6 := (OTHERS => A);

BEGIN
     A1 := ARR1 (A2);          -- ERROR: INCOMPATIBLE COMPONENT TYPES.

     A3 := ARR3 (A1);          -- ERROR: INCOMPATIBLE COMPONENT TYPES.

     N1 := ARR1 (N3);          -- ERROR: INCOMPATIBLE COMPONENT TYPES.

     A2 := ARR2 (A3);          -- ERROR: INCOMPATIBLE COMPONENT TYPES.

     A4 := ARR4 (A5);          -- ERROR: INCOMPATIBLE COMPONENT TYPES.

     A5 := ARR5 (A6);          -- ERROR: INCOMPATIBLE COMPONENT TYPES.
END B46004D;
