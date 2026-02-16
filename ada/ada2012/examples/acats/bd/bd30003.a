-- BD30003.A
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
--     Check that Legality Rules are enforced when Component_Size is
--     specified with an aspect specification.
--
-- TEST DESCRIPTION:
--     The Legality Rules checked herein are:
--        (A) The expression for the Component_Size aspect shall be static -
--            13.3(70).
--        (B) The expression for the Component_Size aspect shall be nonegative
--            - 13.3(70).
--        (C) The expression for the Component_Size aspect cannot freeze the
--            type itself (directly or indirectly) - 13.1(9.2/5), as added by
--            Binding Interpretation AI12-0181-1. (This prevents
--            using attributes, objects, conversions, or qualifications of the
--            type or of a subtype of it.)
--        (D) The expression for the Component_Size aspect of a type given in
--            the visible part of a package cannot name a declaration given
--            in the private part - 13.1.1(11/3).
--        (E) The expression for the Component_Size aspect of a type cannot
--            name a declaration that comes after the freezing point of the
--            type - 13.1.1(13/3).
--        (F) The expression for the Component_Size aspect of a type given in
--            the visible part of a package cannot name a deferred constant,
--            even if the completion of that constant is static -
--            13.14(7.2/3) and 13.14(18).
--        (G) The Component_Size aspect must not be given on a private type
--            even if the full type is an array type - implied by 13.1(9/5).
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
procedure BD30003 is

   Specified_Component : constant := 8;
   Outer_Component : constant := Specified_Component;

   package Pack is
      type Byte is mod 2**8 with
           Size => Specified_Component;               -- ANX-C RQMT. {1:7;1}

      type Check_Array is array (1..10) of Byte
         with Component_Size => Specified_Component;  -- ANX-C RQMT. {1:7;1}

      type Tst1 is array (1..10) of Byte
         with Component_Size => 8;      -- OK. {1:7;1}

      type Tst2 is array (1..10) of Byte
         with Component_Size => ICnst;  -- OK. {1:7;1}

      type Tst3 is array (1..10) of Byte
         with Component_Size => IVar;   -- POSSIBLE ERROR: [Set01] (A) {1:7;1}

      type Tst4 is array (1..10) of Byte
         with Component_Size => -8;     -- ERROR: (B) {1:7;1}

      type Tst5 is array (1..10) of Byte
         with Component_Size =>
                          Neg_Component;-- POSSIBLE ERROR: [Set02] (B) {2:7;1}

      type Tst6 is array (1..8) of Byte
         with Component_Size =>
                        Tst6'Length;    -- ERROR: (C) {2:7;1}

      type Tst7 is array (Positive range <>) of Byte
         with Component_Size =>
                Sub_Tst7'(A_Component); -- POSSIBLE ERROR: [Set03] (C) {2:7;1}
      subtype Sub_Tst7 is
                 Tst7 (1 .. 50);        -- POSSIBLE ERROR: [Set03] (C) {1:7;1}

      type Tst8 is array (1..10) of Byte
         with Component_Size =>
                  Hidden_Component;     -- POSSIBLE ERROR: [Set04] (D) {1:7;1}

      type Tst9 is array (1..10) of Byte
         with Component_Size =>
                      Sml_Component;    -- POSSIBLE ERROR: [Set05] (E) {2:7;1}
      Obj : Tst9 :=
         (others => 2); -- Freezes Tst9 -- POSSIBLE ERROR: [Set05] (E) {1:7;1}

      type TstA is array (1..10) of Byte
         with Component_Size =>
                       Def_Component;   -- POSSIBLE ERROR: [Set06] (F) {2:7;1}

      type TstB is private
         with Component_Size => A_Component;  -- ERROR: (G) {1:7;1}

      type TstC is array (1..10) of Byte
         with Component_Size =>
                    Outer_Component;    -- POSSIBLE ERROR: [Set07] (H) {2:7;1}

      -- Static:
      ICnst : constant Integer := Specified_Component;
      A_Component : constant := Specified_Component;

      Neg_Component : constant :=
               -Specified_Component;    -- POSSIBLE ERROR: [Set02] (B) {1:7;1}

      Sml_Component : constant :=
                   Specified_Component; -- POSSIBLE ERROR: [Set05] (E) {1:7;1}

      ObjC : TstC := (others => 2);     -- Freezes TstC, Component_Size =
                                        --             BD30003.Outer_Component.
                                        -- POSSIBLE ERROR: [Set07] (H) {2:7;1}
      Outer_Component : constant :=
                   Specified_Component; -- POSSIBLE ERROR: [Set07] (H) {1:7;1}
                                        -- Different declaration
                                        -- of Outer_Component.

      -- Not static:
      IVar : Integer :=
                    Specified_Component;-- POSSIBLE ERROR: [Set01] (A) {1:7;1}

      -- Deferred:
      Def_Component : constant Integer; -- POSSIBLE ERROR: [Set06] (F) {7;1}

      -- Resolution recheck here.
   private                              -- POSSIBLE ERROR: [Set07] (H) {1:7;0}

      Hidden_Component : constant :=
                   Specified_Component; -- POSSIBLE ERROR: [Set04] (D) {1:7;1}

      Def_Component : constant Integer
          := Specified_Component;       -- POSSIBLE ERROR: [Set06] (F) {1:7;1}

      type TstB is array (1..10) of Byte;  -- OPTIONAL ERROR: {7;1}
   end Pack;

begin
   null;
end BD30003;
