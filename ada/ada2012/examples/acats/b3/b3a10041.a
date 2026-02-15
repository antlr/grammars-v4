-- B3A10041.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--     See B3A10040.A.
--
-- TEST DESCRIPTION
--     See B3A10040.A.
--
-- TEST FILES:
--     This test consists of the following files:
--         B3A10040.A
--      -> B3A10041.A
--         B3A10042.A
--
-- PASS/FAIL CRITERIA:
--     See B3A10040.A.
--
-- CHANGE HISTORY:
--     29 May 2008  RLB  Created test based on submitted tests.
--!

with Ada.Strings.Unbounded;
with B3A10040;
package B3A10041 is
    type T11 is tagged;                              -- POSSIBLE ERROR: [Set01]
    type T12 is tagged;                              -- POSSIBLE ERROR: [Set02]
    type T13 is tagged;                              -- POSSIBLE ERROR: [Set03]
    type T14 is tagged;                              -- POSSIBLE ERROR: [Set04]
    type T15 is tagged;                              -- POSSIBLE ERROR: [Set05]
    type T16 is tagged;                              -- POSSIBLE ERROR: [Set06]
    type T17 is tagged;                              -- POSSIBLE ERROR: [Set07]
    type T18 is tagged;
    type T19 is tagged;                              -- POSSIBLE ERROR: [Set08]
    type T1A is tagged;
    type T1B is tagged;                              -- POSSIBLE ERROR: [Set09]
    type T1C is tagged;
    type T1D is tagged;
    type T1E is tagged;                              -- POSSIBLE ERROR: [Set10]
    type T1F is tagged;
    type T1G is tagged;                              -- POSSIBLE ERROR: [Set11]

    type T11 is (Red, Green, Blue);                  -- POSSIBLE ERROR: [Set01]
    type T12 is range 1 .. 10;                       -- POSSIBLE ERROR: [Set02]
    type T13 is digits 6;                            -- POSSIBLE ERROR: [Set03]
    type T14 is delta 0.1 range 0.0 .. 10.0;         -- POSSIBLE ERROR: [Set04]
    type T15 is access B3A10040.TPriv;               -- POSSIBLE ERROR: [Set05]
    type T16 is array (1..10) of Character;          -- POSSIBLE ERROR: [Set06]
    type T17 is record B : Boolean; end record;      -- POSSIBLE ERROR: [Set07]
    type T18 is tagged null record;                  -- OK.
    protected type T19 is                            -- POSSIBLE ERROR: [Set08]
       procedure Foo;
    private
       N : Natural;
    end T19;
    protected type T1A is new B3A10040.Interf with   -- OK.
       procedure Foo;
    private
       N : Natural;
    end T1A;
    task type T1B;                                   -- POSSIBLE ERROR: [Set09]
    task type T1C is new B3A10040.Interf with end T1C; -- OK.
    type T1D is limited interface;                   -- OK.
    type T1E is new B3A10040.UPriv;                  -- POSSIBLE ERROR: [Set10]
    type T1F is new B3A10040.TPriv with null record; -- OK.
    type T1G is new Ada.Strings.Unbounded.Unbounded_String; -- POSSIBLE ERROR: [Set11]

end B3A10041;
