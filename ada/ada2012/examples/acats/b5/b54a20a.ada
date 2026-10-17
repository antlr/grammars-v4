-- B54A20A.ADA

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
-- CHECK THAT CHOICE AND EXPRESSION TYPES MUST MATCH IN CASE STATEMENT
-- AND THAT ALL CHOICES MUST BE DISJOINT.

-- DAT 1/22/81

PROCEDURE B54A20A IS

     TYPE INT_1 IS NEW INTEGER RANGE 1 .. 10;
     TYPE INT_2 IS NEW INTEGER RANGE 1 .. 10;

     I1_10     : CONSTANT INT_1 := 10;
     I2_10     : CONSTANT INT_2 := 10;

BEGIN

     CASE INT_1'(4) IS
          WHEN FALSE .. TRUE => NULL;       -- ERROR: WRONG CHOICE TYPE.
          WHEN 1 .. 0 => NULL;              -- OK.
          WHEN INTEGER'(1) .. 0 => NULL;    -- ERROR: WRONG CHOICE TYPE.
          WHEN -1 .. -2 => NULL;            -- OK.
          WHEN INTEGER(1) .. 0 => NULL;     -- ERROR: WRONG CHOICE TYPE.
          WHEN 3 .. 2 => NULL;              -- OK.
          WHEN INT_2'(5) => NULL;           -- ERROR: WRONG CHOICE TYPE.
          WHEN 4 | 2 => NULL;               -- OK.
          WHEN INT_1'(3) => NULL;           -- OK.
          WHEN I1_10 + 0 => NULL;           -- OK.
          WHEN INT_2 RANGE 7 .. 7 => NULL;  -- ERROR: WRONG CHOICE TYPE.
          WHEN INT_1 RANGE 8 .. 8 => NULL;  -- OK.
          WHEN I2_10 - 4 => NULL;           -- ERROR: WRONG CHOICE TYPE.
          WHEN 9 => NULL;                   -- OK.
          WHEN I2_10 => NULL;               -- ERROR: WRONG CHOICE TYPE.
          WHEN OTHERS => NULL;              -- OK.
     END CASE;

     CASE INT_1'(5) IS
          WHEN 7 .. I1_10 => NULL; -- OK.
          WHEN 4 .. 8 => NULL;     -- ERROR: OVERLAPPING CHOICES.
          WHEN OTHERS => NULL;
     END CASE;

     CASE INT_1'(5) IS
          WHEN 7 .. 10 => NULL;
          WHEN 1 .. 4 => NULL;
          WHEN 5 .. 7 => NULL;     -- ERROR: OVERLAPPING CHOICES.
     END CASE;

     CASE INTEGER'(5) IS
          WHEN 1..4 | 10..13 | 3..5 => NULL; -- ERROR: CHOICES OVERLAP.
          WHEN OTHERS => NULL;
     END CASE;

     CASE CHARACTER'('A') IS
          WHEN 'A' => NULL;
          WHEN 'B'..'X' => NULL;
          WHEN 'R'..'T' => NULL;   -- ERROR: OVERLAPPING CHOICES.
          WHEN 'Y' => NULL;        -- OK.
          WHEN 'S' => NULL;        -- ERROR: OVERLAPPING CHOICES.
          WHEN 'U' => NULL;        -- ERROR: OVERLAPPING CHOICES.
          WHEN 'X' => NULL;        -- ERROR: OVERLAPPING CHOICES.
          WHEN 'A' => NULL;        -- ERROR: OVERLAPPING CHOICES.
          WHEN 'Z' => NULL;        -- OK.
          WHEN 'B' => NULL;        -- ERROR: OVERLAPPING CHOICES.
          WHEN OTHERS => NULL;
     END CASE;

     CASE INT_2'(4) IS
          WHEN 1 | 3 | 7 | 5 | 9 | 8 => NULL;
          WHEN 2 | 4 | 6 | 8 | 10 => NULL;  -- ERROR: DUPLICATE CHOICES.
     END CASE;

     CASE INTEGER'(1000) IS
          WHEN 1 .. 4 => NULL;
          WHEN 1004 .. 2007 => NULL;
          WHEN -2007 .. -50 => NULL;
          WHEN -48 .. -43 => NULL;
          WHEN 10 .. 50 => NULL;
          WHEN 123 .. 1001 => NULL;
          WHEN -2015 .. -2008 => NULL;
          WHEN 1504 => NULL;            -- ERROR: OVERLAPPING CHOICES.
          WHEN 2009 => NULL;            -- OK.
          WHEN 7 .. 9 => NULL;          -- OK.
          WHEN 2 .. 3 => NULL;          -- ERROR: OVERLAPPING CHOICES.
          WHEN 999 .. 1003 => NULL;     -- ERROR: OVERLAPPING CHOICES.
          WHEN -49 .. -48 => NULL;      -- ERROR: DUPLICATE CHOICES.
          WHEN OTHERS => NULL;
     END CASE;

END B54A20A;
