-- B35A001.A
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
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVES:
--     Check that Legality Rules are enforced when Small is specified with
--     an aspect specification.
--
-- TEST DESCRIPTION:
--     The Legality Rules checked herein are:
--        (A) The expression for the Small aspect shall be static -
--            3.5.10(2/1).
--        (B) The expression for the Small aspect shall be no greater than
--            equal to the delta - 3.5.9(8/2).
--        (C) The expression for the Small aspect cannot freeze the type
--            itself (directly or indirectly) - 13.1(9.2/5), as added by
--            Binding Interpretation AI12-0181-1. (This prevents
--            using attributes, objects, conversions, or qualifications of the
--            type or of a subtype of it.)
--        (D) The expression for the Small aspect of a type given in the
--            visible part of a package cannot name a declaration given
--            in the private part - 13.1.1(11/3).
--        (E) The expression for the Small aspect of a type cannot name a
--            declaration that comes after the freezing point of the type
--            13.1.1(13/3).
--        (F) The expression for the Small aspect of a type given in the
--            visible part of a package cannot name a deferred constant,
--            even if the completion of that constant is static -
--            13.14(7.2/3) and 13.14(18).
--        (G) The Small aspect must not be given on a private type even
--            if the full type is a fixed point type - implied by 13.1(9/5).
--            [Note: This rule appears to be missing from the RM, it will
--            get added soon; we test it anyway as it follows from the
--            rules for representation items.]
--        (H) The expression for the Small aspect of a type must resolve to
--            the same entities at the first freezing point as at the end of
--            the declaration list - 13.1.1(13/3).
--
--     We try only a handful of examples of each of these rules; we're trying
--     to ensure that a check for the rule exists in the implementation, not
--     that the check is implemented correct in every possible case. Some of
--     the non-specific rules are checked generally for the rule referenced
--     above.
--
-- PASS/FAIL CRITERIA:
--    The test contains several lines marked POSSIBLE ERROR: [Setnn].
--    For each value of nn, the implementation must detect one or more of
--    these possible errors. For instance, an error must be detected on
--    at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--    implementation to pass.
--
-- CHANGE HISTORY:
--     29 Mar 17   RLB     Created test.
--     30 Mar 17   RLB     Added additional checks.
--
--!
procedure B35A001 is

   Outer_Small : constant := 1.0/16.0;

   package Pack is
      type Tst1 is delta 0.5 range -4.0 .. 4.0
         with Small => 0.125;           -- OK. {1:7;1}

      type Tst2 is delta 0.5 range -4.0 .. 4.0
         with Small => FCnst;           -- OK. {1:7;1}

      type Tst3 is delta 0.5 range -4.0 .. 4.0
         with Small => FVar;            -- POSSIBLE ERROR: [Set01] (A) {1:7;1}

      type Tst4 is delta 0.125 range -4.0 .. 4.0
         with Small => 0.5;             -- ERROR: (B) {1:7;1}

      type Tst5 is delta 0.125 range -4.0 .. 4.0
         with Small => Big_Small;       -- POSSIBLE ERROR: [Set02] (B) {1:7;1}

      type Tst6 is delta 0.125 range -4.0 .. 4.0
         with Small => Tst6'Delta;      -- ERROR: (C) {1:7;1}

      type Tst7 is delta 0.125 range -4.0 .. 4.0
         with Small =>
                    Sub_Tst7'(A_Small); -- POSSIBLE ERROR: [Set03] (C) {2:7;1}
      subtype Sub_Tst7 is
                 Tst7 range 0.0 .. 4.0; -- POSSIBLE ERROR: [Set03] (C) {1:7;1}

      type Tst8 is delta 0.5 range -4.0 .. 4.0
         with Small => Hidden_Small;    -- POSSIBLE ERROR: [Set04] (D) {1:7;1}

      type Tst9 is delta 0.5 range -4.0 .. 4.0
         with Small => Sml_Small;       -- POSSIBLE ERROR: [Set05] (E) {1:7;1}
      Obj : Tst9 := 2.0; -- Freezes Tst9-- POSSIBLE ERROR: [Set05] (E) {7;1}

      type TstA is delta 0.5 range -4.0 .. 4.0
         with Small => Def_Small;       -- POSSIBLE ERROR: [Set06] (F) {1:7;1}

      type TstB is private
         with Small => A_Small;         -- ERROR: (G) {1:7;1}

      type TstC is delta 0.5 range -4.0 .. 4.0
         with Small => Outer_Small;     -- POSSIBLE ERROR: [Set07] (H) {1:7;1}

      -- Static:
      FCnst : constant Float := 1.0/8.0;
      A_Small : constant := 1.0/4.0;

      Big_Small : constant := 1.0;      -- POSSIBLE ERROR: [Set02] (B) {7;1}

      Sml_Small : constant := 1.0/8.0;  -- POSSIBLE ERROR: [Set05] (E) {7;1}

      ObjC : TstC := 2.0; -- Freezes TstC, Small = B35A001.Outer_Small.
                                        -- POSSIBLE ERROR: [Set07] (H) {1:7;1}
      Outer_Small : constant := 1.0/8.0;-- POSSIBLE ERROR: [Set07] (H) {7;1}
                                        -- Different declaration of Outer_Small
      -- Not static:
      FVar : Float := 1.0/8.0;          -- POSSIBLE ERROR: [Set01] (A) {7;1}

      -- Deferred:
      Def_Small : constant Float;       -- POSSIBLE ERROR: [Set06] (F) {7;1}

      -- Resolution recheck here.
   private                              -- POSSIBLE ERROR: [Set07] (H) {1:7;0}

      Hidden_Small : constant
                        := 1.0/8.0;     -- POSSIBLE ERROR: [Set04] (D) {1:7;1}

      Def_Small : constant Float
                      := 1.0/8.0;       -- POSSIBLE ERROR: [Set06] (F) {1:7;1}

      type TstB is delta 0.5 range -4.0 .. 4.0; -- OPTIONAL ERROR: {7;1}
   end Pack;

begin
   null;
end B35A001;
