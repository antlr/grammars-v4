-- B4A016A.ADA

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
-- CHECK ILLEGAL FORMS OF EXPRESSIONS WHOSE OPERANDS ARE ALL LITERALS.

-- BAW 29 SEPT 80
-- JBG 5/3/85

PROCEDURE B4A016A IS

     C01A : BOOLEAN := 1.0 < 1;         -- ERROR: OPERAND TYPES.
     C01B : BOOLEAN := 1 < 1.0;         -- ERROR: OPERAND TYPES.
     C02A : BOOLEAN := 1.0 <= 1;        -- ERROR: OPERAND TYPES.
     C02B : BOOLEAN := 1 <= 1.0;        -- ERROR: OPERAND TYPES.
     C03A : BOOLEAN := 1.0 > 1;         -- ERROR: OPERAND TYPES.
     C03B : BOOLEAN := 1 > 1.0;         -- ERROR: OPERAND TYPES.
     C04A : BOOLEAN := 1.0 >= 1;        -- ERROR: OPERAND TYPES.
     C04B : BOOLEAN := 1 >= 1.0;        -- ERROR: OPERAND TYPES.
     C05A : BOOLEAN := 1.0 = 1;         -- ERROR: OPERAND TYPES.
     C05B : BOOLEAN := 1 = 1.0;         -- ERROR: OPERAND TYPES.
     C06A : BOOLEAN := 1.0 /= 1;        -- ERROR: OPERAND TYPES.
     C06B : BOOLEAN := 1 /= 1.0;        -- ERROR: OPERAND TYPES.
     C07A : BOOLEAN := 1 IN 1 .. 1.0;        -- ERROR: OPERAND TYPES.
     C07B : BOOLEAN := 1 NOT IN 1 .. 1.0;    -- ERROR: OPERAND TYPES.
     C08A : BOOLEAN := 1 IN 1.0 .. 1;        -- ERROR: OPERAND TYPES.
     C08B : BOOLEAN := 1 NOT IN 1.0 .. 1;    -- ERROR: OPERAND TYPES.
     C09A : BOOLEAN := 1.0 IN 1 .. 1;        -- ERROR: OPERAND TYPES.
     C09B : BOOLEAN := 1.0 NOT IN 1 .. 1;    -- ERROR: OPERAND TYPES.
     C10A : BOOLEAN := 1.0 IN 1.0 .. 1;      -- ERROR: OPERAND TYPES.
     C10B : BOOLEAN := 1.0 NOT IN 1.0 .. 1;  -- ERROR: OPERAND TYPES.
     C11A : BOOLEAN := 1.0 IN 1 .. 1.0;      -- ERROR: OPERAND TYPES.
     C11B : BOOLEAN := 1.0 NOT IN 1 .. 1.0;  -- ERROR: OPERAND TYPES.
     C12A : BOOLEAN := 1 IN 1.0 .. 1.0;      -- ERROR: OPERAND TYPES.
     C12B : BOOLEAN := 1 NOT IN 1.0 .. 1.0;  -- ERROR: OPERAND TYPES.

     C13  : CONSTANT := 1.0 + 1;             -- ERROR: OPERAND TYPES.
     C14  : CONSTANT := 1 + 1.0;             -- ERROR: OPERAND TYPES.

     C15  : CONSTANT := 1.0 MOD 2;           -- ERROR: OPERAND TYPES.
     C16  : CONSTANT := 2 MOD 3.0;           -- ERROR: OPERAND TYPES.
     C17  : CONSTANT := 1.0 REM 2;           -- ERROR: OPERAND TYPES.
     C18  : CONSTANT := 2 REM 3.0;           -- ERROR: OPERAND TYPES.

     C19  : CONSTANT := 2/1.0;               -- ERROR: OPERAND TYPES.

     C20  : CONSTANT := 1.0**2.0;            -- ERROR: OPERAND TYPES.
     C21  : CONSTANT := 2**2.0;              -- ERROR: OPERAND TYPES.

BEGIN
      NULL;
END B4A016A;
