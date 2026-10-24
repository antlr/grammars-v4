-- B45221A.ADA

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
-- CHECK THAT FOR A DERIVED BOOLEAN TYPE, THE RELATIONAL, EQUALITY, AND
-- MEMBERSHIP OPERATIONS HAVE THE RESULT TYPE PREDEFINED BOOLEAN.

-- JWC 08/20/85

PROCEDURE B45221A IS

     TYPE DERIVED_BOOLEAN IS NEW BOOLEAN;
     D1 , D2, D3 : DERIVED_BOOLEAN := TRUE;

     X1 : DERIVED_BOOLEAN := (D1 = D2) = D3;    -- ERROR: TRUE = D3.

     X1A : DERIVED_BOOLEAN :=
           DERIVED_BOOLEAN'((D1 = D2) = D3);    -- ERROR: TRUE = D3.

     Y1 : BOOLEAN := D1 = D2;                   -- OK.

     Z1 : DERIVED_BOOLEAN := D1 = D2;           -- ERROR: WRONG TYPE.

     X2 : DERIVED_BOOLEAN := (D1 <= D2) <= D3;  -- ERROR: TRUE <= D3.

     Y2 : BOOLEAN := D1 <= D2;                  -- OK.

     Z2 : DERIVED_BOOLEAN := D1 <= D2;          -- ERROR: WRONG TYPE.

     X3 : DERIVED_BOOLEAN := (D1 < D2) < D3;    -- ERROR: FALSE < D3.

     X3A : DERIVED_BOOLEAN :=
           DERIVED_BOOLEAN'((D1 < D2) < D3);    -- ERROR: FALSE < D3.

     Y3 : BOOLEAN := D1 < D2;                   -- OK.

     Z3 : DERIVED_BOOLEAN := D1 < D2;           -- ERROR: WRONG TYPE.

     X4 : DERIVED_BOOLEAN := (D1 >= D2) >= D3;  -- ERROR: TRUE >= D3.

     X4A : DERIVED_BOOLEAN :=
           DERIVED_BOOLEAN'((D1 >= D2) >= D3);  -- ERROR: TRUE >= D3.

     Y4 : BOOLEAN := D1 >= D2;                  -- OK.

     Z4 : DERIVED_BOOLEAN := D1 >= D2;          -- ERROR: WRONG TYPE.

     X5 : DERIVED_BOOLEAN := (D1 > D2) > D3;    -- ERROR: FALSE > D3.

     Y5 : BOOLEAN := D1 > D2;                   -- OK.

     Z5 : DERIVED_BOOLEAN := D1 > D2;           -- ERROR: WRONG TYPE.

     X6 : DERIVED_BOOLEAN := (D1 /= D2) /= D3;  -- ERROR: FALSE /= D3.

     Y6 : BOOLEAN := D1 /= D2;                  -- OK.

     Z6 : DERIVED_BOOLEAN := D1 /= D2;          -- ERROR: WRONG TYPE.


     PROCEDURE P1 (B : DERIVED_BOOLEAN ) IS
     BEGIN
          NULL;
     END P1;

     PROCEDURE P2 (B : BOOLEAN ) IS
     BEGIN
          NULL;
     END P2;

BEGIN

     P1 (D1 IN DERIVED_BOOLEAN);             -- ERROR: WRONG TYPE.
     P2 (D1 IN DERIVED_BOOLEAN);             -- OK.
     P1 (D1 NOT IN DERIVED_BOOLEAN);         -- ERROR: WRONG TYPE.
     P2 (D1 NOT IN DERIVED_BOOLEAN);         -- OK.

END B45221A;
