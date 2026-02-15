-- B46002A.ADA

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
-- CHECK THAT THE OPERAND OF A TYPE CONVERSION MUST NOT BE
-- AN ALLOCATOR, AN AGGREGATE, A STRING LITERAL, OR ANY OF THE
-- PRECEDING ENCLOSED IN ONE OR MORE SETS OF PARENTHESES.

-- R.WILLIAMS 9/5/86
-- 03/16/07 - RLB - Removed null cases, as these are legal by
--                  Amendment 1.
PROCEDURE B46002A IS

     TYPE T1 IS ARRAY (POSITIVE RANGE <>) OF INTEGER;
     AT1 : T1 (1 .. 2) := (OTHERS => 0);

     SUBTYPE ST1 IS T1 (1 .. 1);

     TYPE ACC IS ACCESS T1;
     AC1 : ACC;

     TYPE T2 IS ARRAY (POSITIVE RANGE <>) OF CHARACTER;
     AT2 : T2 (1 .. 2) := (OTHERS => 'A');

BEGIN
     AC1 := ACC (NEW ST1);                      -- ERROR: ALLOCATOR.
     AC1 := ACC ((NEW ST1));                    -- ERROR: ALLOCATOR.
     AC1 := ACC (((NEW ST1)));                  -- ERROR: ALLOCATOR.
     AC1 := ACC (NEW ST1'(1 ..1 => 0));         -- ERROR: ALLOCATOR.
     AC1 := ACC ((NEW ST1'(1 ..1 => 0)));       -- ERROR: ALLOCATOR.
     AC1 := ACC (((NEW ST1'(1 ..1 => 0))));     -- ERROR: ALLOCATOR.
     AC1 := ACC (NEW T1 (1 .. 2));              -- ERROR: ALLOCATOR.
     AC1 := ACC ((NEW T1 (1 .. 2)));            -- ERROR: ALLOCATOR.
     AC1 := ACC (((NEW T1 (1 .. 2))));          -- ERROR: ALLOCATOR.

     AT1 := T1 ((1, 2));                        -- ERROR: AGGREGATE.
     AT1 := T1 (((1, 2)));                      -- ERROR: AGGREGATE.
     AT1 := T1 ((((1, 2))));                    -- ERROR: AGGREGATE.
     AT1 := T1 ((1 => 0, 2 => 0));              -- ERROR: AGGREGATE.
     AT1 := T1 (((1 => 0, 2 => 0)));            -- ERROR: AGGREGATE.
     AT1 := T1 ((((1 => 0, 2 => 0))));          -- ERROR: AGGREGATE.

     AT2 := T2 (('A', 'B'));                    -- ERROR: AGGREGATE.
     AT2 := T2 ((('A', 'B')));                  -- ERROR: AGGREGATE.
     AT2 := T2 (((('A', 'B'))));                -- ERROR: AGGREGATE.
     AT2 := T2 ((1 => 'C', 2 => 'D'));          -- ERROR: AGGREGATE.
     AT2 := T2 (((1 => 'C', 2 => 'D')));        -- ERROR: AGGREGATE.
     AT2 := T2 ((((1 => 'C', 2 => 'D'))));      -- ERROR: AGGREGATE.

     AT2 := T2 ("AB");                          -- ERROR: STRING LIT.
     AT2 := T2 (("AB"));                        -- ERROR: STRING LIT.
     AT2 := T2 ((("AB")));                      -- ERROR: STRING LIT.

END B46002A;
