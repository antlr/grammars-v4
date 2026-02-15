-- BD30002.A
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
--     Check that Legality Rules are enforced when Alignment for a type is
--     specified with an aspect specification.
--
-- TEST DESCRIPTION:
--     The Legality Rules checked herein are:
--        (A) The expression for the Alignment aspect shall be static -
--            13.3(26.4/2).
--        (B) The expression for the Alignment aspect shall be nonegative -
--            13.3(26.4/2).
--        (C) The expression for the Alignment aspect cannot freeze the type
--            itself (directly or indirectly) - 13.1(9.2/5), as added by
--            Binding Interpretation AI12-0181-1. (This prevents
--            using attributes, objects, conversions, or qualifications of the
--            type or of a subtype of it.)
--        (D) The expression for the Alignment aspect of a type given in the
--            visible part of a package cannot name a declaration given
--            in the private part - 13.1.1(11/3).
--        (E) The expression for the Alignment aspect of a type cannot name a
--            declaration that comes after the freezing point of the type
--            13.1.1(13/3).
--        (F) The expression for the Alignment aspect of a type given in the
--            visible part of a package cannot name a deferred constant,
--            even if the completion of that constant is static -
--            13.14(7.2/3) and 13.14(18).
--        (G) The Alignment aspect must not be given on a private type -
--            implied by 13.1(9/5).
--            [Note: This rule appears to be missing from the RM, it will
--            get added soon; we test it anyway as it follows from the
--            rules for representation items.]
--        (H) The expression for the Alignment aspect of a type must resolve to
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
-- APPLICABILITY CRITERIA:
--     All implementations must attempt to compile this test.
--
--     For implementations validating against Systems Programming Annex (C):
--        this test must detect all of the errors as marked.
--
--     or implementations not validating against Annex C:
--        this test may report compile time errors at one or more points
--        indicated by "-- ANX-C RQMT", in which case it may be graded as
--        inapplicable. Otherwise, the test all of the errors as marked.
--
-- CHANGE HISTORY:
--     29 Mar 17   RLB     Created test.
--     30 Mar 17   RLB     Added additional checks.
--
--!
procedure BD30002 is

   type Basic_Int is range -99 .. 99;
   Specified_Alignment : constant := Basic_Int'Alignment;

   Outer_Alignment : constant := Specified_Alignment;

   package Pack is

      type Check_Int is range -99 .. 99
         with Alignment => Specified_Alignment;         -- ANX-C RQMT. {1:7;1}

      type Tst1 is range -99 .. 99
         with Alignment => 1;                           -- ANX-C RQMT. {1:7;1}

      type Tst2 is range -99 .. 99
         with Alignment => ICnst;        -- OK. {1:7;1}

      type Tst3 is range -99 .. 99
         with Alignment => IVar;         -- POSSIBLE ERROR: [Set01] (A) {1:7;1}

      type Tst4 is range -99 .. 99
         with Alignment => -10;          -- ERROR: (B) {1:7;1}

      type Tst5 is range -99 .. 99
         with Alignment => Neg_Alignment;-- POSSIBLE ERROR: [Set02] (B) {1:7;1}

      type Tst6 is range -99 .. 99
         with Alignment => Tst6'Alignment; -- ERROR: (C) {1:7;1}

      type Tst7 is range -99 .. 99
         with Alignment =>
                Sub_Tst7'(An_Alignment); -- POSSIBLE ERROR: [Set03] (C) {2:7;1}
      subtype Sub_Tst7 is
                  Tst7 range 0 .. 50;    -- POSSIBLE ERROR: [Set03] (C) {1:7;1}

      type Tst8 is range -99 .. 99
         with Alignment =>
                   Hidden_Alignment;     -- POSSIBLE ERROR: [Set04] (D) {2:7;1}

      type Tst9 is range -99 .. 99
         with Alignment => Sml_Alignment;-- POSSIBLE ERROR: [Set05] (E) {1:7;1}
      Obj : Tst9 := 2; -- Freezes Tst9   -- POSSIBLE ERROR: [Set05] (E) {7;1}

      type TstA is range -99 .. 99
         with Alignment => Def_Alignment;-- POSSIBLE ERROR: [Set06] (F) {1:7;1}

      type TstB is private
         with Alignment => An_Alignment; -- ERROR: (G) {1:7;1}

      type TstC is range -99 .. 99
         with Alignment =>
                   Outer_Alignment;      -- POSSIBLE ERROR: [Set07] (H) {2:7;1}

      -- Static:
      ICnst : constant Integer := Specified_Alignment;
      An_Alignment : constant := Specified_Alignment;

      Neg_Alignment : constant :=
               -Specified_Alignment;     -- POSSIBLE ERROR: [Set02] (B) {1:7;1}

      Sml_Alignment : constant :=
                   Specified_Alignment;  -- POSSIBLE ERROR: [Set05] (E) {1:7;1}

      ObjC : TstC := 2; -- Freezes TstC, Alignment = BD30002.Outer_Alignment.
                                         -- POSSIBLE ERROR: [Set07] (H) {1:7;1}
      Outer_Alignment : constant :=
                  Specified_Alignment;   -- POSSIBLE ERROR: [Set07] (H) {1:7;1}
                                         -- Different declaration
                                         -- of Outer_Alignment.

      -- Not static:
      IVar : Integer :=
                     Specified_Alignment;-- POSSIBLE ERROR: [Set01] (A) {1:7;1}

      -- Deferred:
      Def_Alignment : constant Integer;  -- POSSIBLE ERROR: [Set06] (F) {7;1}

      -- Resolution recheck here.
   private                               -- POSSIBLE ERROR: [Set07] (H) {1:7;0}

      Hidden_Alignment : constant :=
                   Specified_Alignment;  -- POSSIBLE ERROR: [Set04] (D) {1:7;1}

      Def_Alignment : constant Integer
          := Specified_Alignment;        -- POSSIBLE ERROR: [Set06] (F) {1:7;1}

      type TstB is range -99 .. 99;      -- OPTIONAL ERROR: {7;1}
   end Pack;

begin
   null;
end BD30002;
