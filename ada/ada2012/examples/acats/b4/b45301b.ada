-- B45301B.ADA

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
-- DIFFERENT FLOATING POINT TYPES OR FOR OPERANDS OF MIXED FLOATING
-- AND INTEGER TYPES.

-- RJW 2/8/86

PROCEDURE B45301B IS
     
BEGIN

     DECLARE
          TYPE T1 IS DIGITS 3 RANGE -1.0 .. 1.0;
          TYPE T2 IS DIGITS 3 RANGE -1.0 .. 1.0;

          F1 : T1 := 0.0;
          F2 : T2 := 0.0;
     BEGIN
          F1  := F1 + F2;          -- ERROR: DIFFERENT TYPES FOR '+'.
          F1  := F1 - F2;          -- ERROR: DIFFERENT TYPES FOR '-'.
          F1  := F1 + 1;           -- ERROR: DIFFERENT TYPES FOR '+'.
          F1  := F1 - 1;           -- ERROR: DIFFERENT TYPES FOR '-'.
     END;

     DECLARE

          TYPE T1 IS DIGITS 3 RANGE -1.0 .. 1.0;
          TYPE T2 IS NEW T1;

          F1 : T1 := 0.0;
          F2 : T2 := 0.0;
     BEGIN
          F1 := F1 + F2;         -- ERROR: DIFFERENT TYPES FOR '+'.
          F1 := F1 - F2;         -- ERROR: DIFFERENT TYPES FOR '-'.
     END;

END B45301B;
