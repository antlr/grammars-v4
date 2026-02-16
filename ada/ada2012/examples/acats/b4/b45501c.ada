-- B45501C.ADA

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
--     THE OPERATORS MOD AND REM ARE NOT PREDEFINED WHEN BOTH OPERANDS
--     ARE REAL OR WHEN THE FIRST OPERAND IS REAL AND THE SECOND IS
--     AN INTEGER.

-- HISTORY:
--     BCB 07/13/88  CREATED ORIGINAL TEST.

PROCEDURE B45501C IS

     TYPE FIX IS DELTA 2.0**(-1) RANGE -2.0 .. 2.0;

     A : INTEGER := 1;

     B, C, D : FLOAT := 1.0;

     E, F, G : FIX := 1.0;

BEGIN

     D := B REM C;                        -- ERROR: FLOAT REM FLOAT.
     D := B MOD C;                        -- ERROR: FLOAT MOD FLOAT.

     D := B REM A;                        -- ERROR: FLOAT REM INTEGER.
     D := B MOD A;                        -- ERROR: FLOAT MOD INTEGER.

     G := E REM F;                        -- ERROR: FIXED REM FIXED.
     G := E MOD F;                        -- ERROR: FIXED MOD FIXED.

     G := E REM A;                        -- ERROR: FIXED REM INTEGER.
     G := E MOD A;                        -- ERROR: FIXED MOD INTEGER.

END B45501C;
