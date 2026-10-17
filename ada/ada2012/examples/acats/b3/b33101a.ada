-- B33101A.ADA

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
-- CHECK THAT ENUMERATION, INTEGER, REAL, ARRAY, ACCESS, AND DERIVED
-- TYPE_DECLARATIONS CANNOT HAVE DISCRIMINANT PARTS.

-- JRK 4/1/81
-- JWC 10/4/85  RENAMED FROM B33002A.ADA;
--              ADDED TESTS FOR ACCESS AND DERIVED TYPES.
-- RLB 11/19/19  Added error location indicators.

PROCEDURE B33101A IS

     SUBTYPE S IS INTEGER RANGE 0..10;

     TYPE E (D : S) IS (E1, E2);        -- ERROR: DISCRIMINANT_PART.   {6}
     TYPE I (D : S) IS RANGE 0..10;     -- ERROR: DISCRIMINANT_PART.   {6}
     TYPE R (D : S) IS DIGITS 3;        -- ERROR: DISCRIMINANT_PART.   {6}
     TYPE F (D : S) IS DELTA 1.0
                       RANGE 0.0 .. 3.0;-- ERROR: DISCRIMINANT_PART. {1:6}
     TYPE A (D : S) IS ARRAY (S) OF S;  -- ERROR: DISCRIMINANT_PART.   {6}
     TYPE P (D : S) IS ACCESS S;        -- ERROR: DISCRIMINANT_PART.   {6}
     TYPE N (D : S) IS NEW INTEGER;     -- ERROR: DISCRIMINANT_PART.   {6}

BEGIN
     NULL;
END B33101A;
