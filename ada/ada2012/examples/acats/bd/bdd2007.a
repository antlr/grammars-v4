-- BDD2007.A
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
--     Check that Legality Rules are enforced when Stream_Size is
--     specified with an aspect specification.
--
-- TEST DESCRIPTION:
--     The Legality Rules checked herein are:
--        (A) The expression for the Stream_Size aspect shall be an integer
--            type - 13.13.2(1.4-1.5/2).
--        (B) The expression for the Stream_Size aspect shall be static -
--            13.13.2(1.5/2).
--        (C) The expression for the Stream_Size aspect shall be nonegative -
--            13.13.2(1.5/2).
--        (D) The expression for the Stream_Size aspect shall be a multiple of
--            Ada.Streams.Stream_Element'Size - 13.13.2(1.5/2).
--        (E) The expression for the Stream_Size aspect shall be no less than
--            the size of the first subtype - 13.13.2(1.8/2). Note: While this
--            rule is part of Implementation Advice, it is clearly necessary
--            to be able to represent every possible value of the first
--            subtype. So long as we are careful to ensure that there are more
--            values than the specified stream size would have, we can test
--            this.
--        (F) The Stream_Size aspect shall be specified on an elementary
--            type - 13.13.2(1.1/2).
--        (G) The Stream_Size aspect shall be specified on a first subtype
--            13.13.2(1.5/2).
--        (H) The expression for the Stream_Size aspect cannot freeze the type
--            itself (directly or indirectly) - 13.1(9.1/5), as added by
--            Binding Interpretation AI12-0181-1. (This prevents
--            using attributes, objects, conversions, or qualifications of the
--            type or of a subtype of it.)
--        (I) The expression for the Stream_Size aspect of a type given in the
--            visible part of a package cannot name a declaration given
--            in the private part - 13.1.1(11/3).
--        (J) The expression for the Stream_Size aspect of a type cannot name a
--            declaration that comes after the freezing point of the type
--            13.1.1(13/3).
--        (K) The expression for the Stream_Size aspect of a type must resolve
--            to the same entities at the first freezing point as at the end of
--            the declaration list - 13.1.1(13/3).
--
--     We try only a handful of examples of each of these rules; we're trying
--     to ensure that a check for the rule exists in the implementation, not
--     that the check is implemented correctly in every possible case. Some of
--     the non-specific rules are checked generally for the rule referenced
--     above.
--
--     Note: We assume here that Stream_Element'Size is less than 13.
--     In the very unlikely event that this is not true for an implementation,
--     the implementer should petition the ACAA for a grading modification.
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
--     10 Mar 20   RLB     Created test from existing BD30001 for Size.
--
--!
with Ada.Streams;
procedure BDD2007 is

   Stream_Element_Size : constant := Ada.Streams.Stream_Element'Size;
   type Basic_Int is range -99 .. 99;
   Basic_Stream_Size : constant := Basic_Int'Stream_Size;
   type Larger_Int is range -9999 .. 9999;
   Larger_Stream_Size : constant := Larger_Int'Stream_Size;
   type Basic_Mod is mod 2 ** 8;
   Mod_Stream_Size : constant := Basic_Mod'Stream_Size;

   Outer_Stream_Size : constant := Basic_Stream_Size;

   package Pack is

      type Check_Int is range -99 .. 99
         with Stream_Size => Basic_Stream_Size;         -- ANX-C RQMT. {1:7;1}

      type Tst01 is range -99 .. 99
         with Stream_Size => Larger_Stream_Size;        -- ANX-C RQMT. {1:7;1}

      type Tst02 is mod 2 ** 8
         with Stream_Size => Mod_Stream_Size;           -- ANX-C RQMT. {1:7;1}

      type Tst03 is range -99 .. 99
         with Stream_Size => True;       -- ERROR: (A) {1:7;1}

      type Tst04 is range -99 .. 99
         with Stream_Size => 8.0;        -- ERROR: (A) {1:7;1}

      type Tst05 is range -99 .. 99
         with Stream_Size => ICnst;      -- OK. {1:7;1}

      type Tst06 is range -99 .. 99
         with Stream_Size => IVar;       -- POSSIBLE ERROR: [Set01] (B) {1:7;1}

      type Tst07 is range -99 .. 99
         with Stream_Size => -16;        -- ERROR: (C) {1:7;1}

      type Tst08 is mod 2 ** 8
         with Stream_Size => Neg_Size;   -- POSSIBLE ERROR: [Set02] (C) {1:7;1}

      type Tst09 is range -99 .. 99
         with Stream_Size => 13;         -- ERROR: (D) {1:7;1}

      type Tst10 is mod 2 ** 8
         with Stream_Size => Calc_Size;  -- POSSIBLE ERROR: [Set03] (D) {1:7;1}

      type Tst11 is range -9999 .. 9999
         with Stream_Size => Basic_Stream_Size; -- ERROR: (E) {1:7;1}

      type Tst12 is record
          C : Check_Int;
      end record with Stream_Size => Basic_Stream_Size; -- ERROR: (F) {2:7;1}

      type Tst13 is private
         with Stream_Size => ICnst;           -- ERROR: (F) {1:7;1}
         -- Note: all private types are composite. Also, 13.1(9.2/5) makes
         -- this illegal for all representation aspects.

      type Tst14 is mod 2 ** 8;
      subtype S14 is Tst14
         with Stream_Size => Mod_Stream_Size; -- ERROR: (G) {1:7;1}

      type Tst15 is range -99 .. 99
         with Stream_Size => Tst15'Size;      -- ERROR: (H) {1:7;1}

      A_Size : constant := Larger_Stream_Size;
      type Tst16 is mod 2 ** 8
         with Stream_Size =>
                    Sub_Tst16'(A_Size);  -- POSSIBLE ERROR: [Set04] (H) {2:7;1}
      subtype Sub_Tst16 is
                Tst16 range 0 .. 50;     -- POSSIBLE ERROR: [Set04] (H) {1:7;1}

      type Tst17 is mod 2 ** 8
         with Stream_Size => Hidden_Size;-- POSSIBLE ERROR: [Set05] (I) {1:7;1}

      type Tst18 is range -99 .. 99
         with Stream_Size => Sml_Size;   -- POSSIBLE ERROR: [Set06] (J) {1:7;1}
      Obj : Tst18 := 2; -- Freezes Tst18 -- POSSIBLE ERROR: [Set06] (J) {7;1}

      type Tst19 is range -99 .. 99 with
         Stream_Size => Outer_Stream_Size;-- POSSIBLE ERROR: [Set07] (K){1:7;1}

      -- Static:
      ICnst : constant Integer := Basic_Stream_Size;

      Neg_Size : constant :=
               -Mod_Stream_Size;         -- POSSIBLE ERROR: [Set02] (C) {1:7;1}

      Calc_Size : constant :=
               Mod_Stream_Size-1;        -- POSSIBLE ERROR: [Set03] (D) {1:7;1}

      Sml_Size : constant :=
                      Basic_Stream_Size; -- POSSIBLE ERROR: [Set06] (J) {1:7;1}

      Obj19 : Tst19 := 2; -- Freezes Tst19,
                            -- Stream_Size = BDD2007.Outer_Stream_Size.
                                         -- POSSIBLE ERROR: [Set07] (K) {2:7;1}
      Outer_Stream_Size : constant :=
                      Basic_Stream_Size; -- POSSIBLE ERROR: [Set07] (K) {1:7;1}
                            -- Different declaration of Outer_Stream_Size.

      -- Not static:
      IVar : Integer := Basic_Stream_Size; -- POSSIBLE ERROR: [Set01] (B) {7;1}

      -- Resolution recheck here.
   private                               -- POSSIBLE ERROR: [Set07] (K) {1:7;0}
      Hidden_Size : constant :=
                      Mod_Stream_Size;   -- POSSIBLE ERROR: [Set05] (I) {1:7;1}

      type Tst13 is range -99 .. 99;     -- OPTIONAL ERROR: {7;1}

   end Pack;

begin
   null;
end BDD2007;
