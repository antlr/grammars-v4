-- B36201A.ADA

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
-- CHECK THAT ARRAY ATTRIBUTES FIRST, FIRST(#), LAST,
-- LAST(#), LENGTH, LENGTH(#), RANGE, RANGE(#) CANNOT BE
-- APPLIED TO UNCONSTRAINED ARRAY TYPES, OR TO RECORD
-- OR PRIVATE TYPES, AND THAT NO PARAMETER CAN BE PRESENT
-- WHEN AN ATTRIBUTE IS APPLIED TO A SCALAR TYPE OR SCALAR
-- OBJECT.
-- ALSO, CHECK THAT ONLY ONE ARGUMENT IS PERMITTED
-- (WHICH MUST BE STATIC OF SOME INTEGER TYPE AND IN RANGE).

-- DAT 2/11/81
-- ABW 6/9/82
-- JBG 6/9/83
-- RJW 1/21/86 REVISED.  ADDED TESTS FOR A2'BASE'FIRST, ETC.
-- PWN 11/05/95  REMOVED CHECKS WHERE UNIVERSAL INTEGER WAS FORMERLY REQUIRED.
-- PWN 03/21/95  Restored checks in Ada 95 legal format.
-- RLB 03/13/03  'BASE is illegal for arrays in Ada 95. Thus, the test uses of
--               that attribute are not testing the objective, and they have
--               been removed.

PROCEDURE B36201A IS

     TYPE I_1 IS NEW INTEGER RANGE 1 .. 1;
     TYPE UA IS ARRAY (I_1 RANGE <> ) OF I_1;
     I1 : I_1 := 1;
     C1 : CONSTANT I_1 := 1;
     TYPE A2 IS ARRAY (I_1, I_1) OF I_1;
     B : BOOLEAN;
     I : INTEGER;

     TYPE R IS RECORD
          E : A2;
     END RECORD;

     PACKAGE P IS
          TYPE PVT IS PRIVATE;
     PRIVATE
          TYPE PVT IS NEW A2;
     END P;

     USE P;

BEGIN

     I1 := A2'LENGTH;                        -- OK.
     I1 := A2'FIRST;                         -- OK.
     I1 := UA'LENGTH;                        -- ERROR: UNCONSTRAINED.
     I1 := UA'FIRST;                         -- ERROR: UNCONSTRAINED.
     I1 := UA'LAST;                          -- ERROR: UNCONSTRAINED.
     I1 := 1;
     I := 1;
     IF I1 IN UA'RANGE THEN                  -- ERROR: UNCONSTRAINED.
          NULL;
     END IF;
     IF I1 = UA'FIRST(1) THEN                -- ERROR: UNCONSTRAINED.
          NULL;
     END IF;
     IF I1 = I1'FIRST THEN                   -- ERROR: I1 SCALAR.
          NULL;
     END IF;
     IF 6 IN INTEGER'RANGE THEN              -- OK.
          NULL;
     END IF;
     IF 6 = INTEGER'FIRST(1) THEN            -- ERROR: SCALAR.
          NULL;
     END IF;
     IF 1 = INTEGER'LENGTH THEN              -- ERROR: SCALAR.
          NULL;
     END IF;
     B := I1 = I_1'FIRST;                    -- OK.
     B := I1 = I_1'LAST;                     -- OK.
     B := I1 = I_1'FIRST(1);                 -- ERROR: I_1 SCALAR.
     B := I1 = I_1'LAST(1);                  -- ERROR: I_1 SCALAR.
     B := I = I_1'LENGTH;                    -- ERROR: I_1 SCALAR.
     B := I = I_1'LENGTH(1);                 -- ERROR: I_1 SCALAR.
     B := I1 IN I_1'RANGE;                   -- OK.
     B := I1 IN I_1'RANGE(1);                -- ERROR: I_1 SCALAR.
     B := I = I'LAST;                        -- ERROR: I SCALAR.
     B := I IN I'RANGE;                      -- ERROR: I IS SCALAR.
     B := I = I'LENGTH;                      -- ERROR: I IS SCALAR.
     I := STRING'FIRST;                      -- ERROR: UNCONSTRAINED.
     I := R'FIRST;                           -- ERROR: R RECORD.
     B := R'FIRST IN R'RANGE;                -- ERROR: R RECORD.
     B := I = R'LENGTH(1);                   -- ERROR: R RECORD.
     B := I IN PVT'RANGE;                    -- ERROR: PVT PRIVATE.
     B := I < PVT'LENGTH;                    -- ERROR: PVT PRIVATE.
     I := A2'RANGE(1,1);                     -- ERROR: 2 ARGS.
     I := A2'LENGTH(I1);                     -- ERROR: NOT STATIC/UNIV.
     I1 := A2'FIRST(A2'LENGTH);              -- OK.
     I1 := A2'FIRST(C1 + 1);                 -- OK.
     I1 := A2'FIRST(I_1'POS(C1 + 1));        -- OK.
     B := I1 = A2'LAST(0);                   -- ERROR: 0 ILLEGAL.
     B := I1 = A2'LAST(2);                   -- OK.
     B := I1 = A2'LAST(3);                   -- ERROR: 3 ILLEGAL.
     B := I1 IN A2'RANGE(0);                 -- ERROR: 0 ILLEGAL.
     I := A2'LENGTH(-1);                     -- ERROR: -1 ILLEGAL.

END B36201A;
