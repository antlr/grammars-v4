-- B41201C.ADA

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
-- CHECK THAT THE BASE TYPE OF THE DISCRETE RANGE OF A SLICE MUST BE
--   THE SAME AS THE BASE TYPE OF THE INDEX OF THE ARRAY TYPE.

-- WKB 8/10/81
-- JWC 6/28/85   RENAMED TO -AB

PROCEDURE B41201C IS

     SUBTYPE S IS INTEGER RANGE 5..50;
     TYPE S1 IS NEW S;
     X : S1 := 6;
     Y : S1 := 9;

     TYPE T IS ARRAY (INTEGER RANGE <> ) OF INTEGER;
     A : T(0..50) := (0..50 => 1);
     B : T(1..4);
     C : T(1..2);

     TYPE U IS (E1,E2,E3,E4);
     TYPE U1 IS ARRAY (U RANGE <> ) OF INTEGER;
     D : U1(E1..E4) := (E1..E4 => 3);
     E : U1(E2..E3);

BEGIN

     B := A (X..Y);                    -- ERROR: DIFFERENT BASE TYPES.
     NULL;
     B := A (ASCII.LF .. ASCII.CR);    -- ERROR: DIFFERENT BASE TYPES.
     NULL;
     C := A (FALSE..TRUE);             -- ERROR: DIFFERENT BASE TYPES.
     NULL;
     C := A (E2..E3);                  -- ERROR: DIFFERENT BASE TYPES.
     NULL;
     E := D (2..3);                    -- ERROR: DIFFERENT BASE TYPES.

END B41201C;
