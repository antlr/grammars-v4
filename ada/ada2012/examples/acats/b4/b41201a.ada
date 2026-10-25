-- B41201A.ADA

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
-- CHECK THAT MORE THAN ONE DISCRETE RANGE IS FORBIDDEN IN SLICES,
--   EVEN FOR MULTIDIMENSIONAL ARRAYS.
-- CHECK THAT ONE DISCRETE RANGE CANNOT BE GIVEN FOR A 
--   MULTIDIMENSIONAL ARRAY.

-- WKB 8/10/81
-- SPS 11/23/82

PROCEDURE B41201A IS

     TYPE T IS ARRAY (INTEGER RANGE <> ) OF INTEGER;
     SUBTYPE T1 IS T(1..10);
     A : T1 := (1..10 => 1);

     TYPE U IS ACCESS T1;
     B : U := NEW T1 '(1..10 => 2);

     TYPE V IS ARRAY (INTEGER RANGE <>, INTEGER RANGE <> ) OF INTEGER;
     C : V(1..10, 1..10) := (1..10 => (1..10 => 3));

     TYPE W IS ACCESS V;
     D : W := NEW V' (1..10 => (1..10 => 4));

     I : T(1..5);
     J : V(1..5, 1..5);
     K : V(1..5, 1..10);

     FUNCTION F RETURN T1 IS
     BEGIN
          RETURN (1..10 => 5);
     END F;

     FUNCTION G RETURN V IS
     BEGIN
          RETURN (1..10 => (1..10 => 5));
     END G;

BEGIN

     I := A (1..5, 9);             -- ERROR: IMPROPER SLICE.
     NULL;
     I := A (5, 3..7);             -- ERROR: IMPROPER SLICE.
     NULL;
     I := A (6..10, 3..7);         -- ERROR: IMPROPER SLICE.
     NULL;
     I := B (1..5, 9);             -- ERROR: IMPROPER SLICE.
     NULL;
     I := B (5, 3..7);             -- ERROR: IMPROPER SLICE.
     NULL;
     I := B (6..10, 3..7);         -- ERROR: IMPROPER SLICE.
     NULL;
     I := F(1..5, 9);            -- ERROR: IMPROPER SLICE.
     NULL;
     I := F(5, 3..7);            -- ERROR: IMPROPER SLICE.
     NULL;
     I := F(6..10, 3..7);        -- ERROR: IMPROPER SLICE.
     NULL;
     K := C (2..6);                -- ERROR: IMPROPER SLICE.
     NULL;
     I := C (4..8, 8);             -- ERROR: IMPROPER SLICE.
     NULL;
     I := C (4, 6..10);            -- ERROR: IMPROPER SLICE.
     NULL;
     J := C (3..7, 1..5);          -- ERROR: IMPROPER SLICE.
     NULL;
     K := D (2..6);                -- ERROR: IMPROPER SLICE.
     NULL;
     I := D (4..8, 8);             -- ERROR: IMPROPER SLICE.
     NULL;
     I := D (4, 6..10);            -- ERROR: IMPROPER SLICE.
     NULL;
     J := D (3..7, 1..5);          -- ERROR: IMPROPER SLICE.
     NULL;
     K := G(2..6);               -- ERROR: IMPROPER SLICE.
     NULL;
     I := G(4..8, 8);            -- ERROR: IMPROPER SLICE.
     NULL;
     I := G(4, 6..10);           -- ERROR: IMPROPER SLICE.
     NULL;
     J := G(3..7, 1..5);         -- ERROR: IMPROPER SLICE.

END B41201A;
