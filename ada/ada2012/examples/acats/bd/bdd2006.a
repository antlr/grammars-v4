-- BDD2006.A
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
--     specified with an attribute definition clause.
--
-- TEST DESCRIPTION:
--     The Legality Rules checked herein are:
--        (A) The expression for the Stream_Size attribute shall be an integer
--            type - 13.13.2(1.4-1.5/2).
--        (B) The expression for the Stream_Size attribute shall be static -
--            13.13.2(1.5/2).
--        (C) The expression for the Stream_Size attribute shall be nonegative
--            - 13.13.2(1.5/2).
--        (D) The expression for the Stream_Size attribute shall be a multiple
--            of Ada.Streams.Stream_Element'Size - 13.13.2(1.5/2).
--        (E) The expression for the Stream_Size attribute shall be no less
--            than the size of the first subtype - 13.13.2(1.8/2). Note:
--            While this rule is part of Implementation Advice, it is clearly
--            necessary to be able to represent every possible value of the
--            first subtype. So long as we are careful to ensure that there
--            are more values than the specified stream size would have, we can
--            test this.
--        (F) The Stream_Size attribute shall be specified on an elementary
--            type - 13.13.2(1.1/2).
--        (G) The Stream_Size attribute shall be specified on a first subtype
--            13.13.2(1.5/2).
--        (H) The Stream_Size attribute shall be specified before the type is
--            frozen - 13.1(9/5). A special case of this is when the expression
--            of the attribute definition clause freezes the type itself.
--        (I) The expression for the Stream_Size attribute of a type must
--            resolve at the point it is given (it is not special like aspect
--            specifications).
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
--     19 Mar 20   RLB     Created test from previous constructed BDD2007.
--
--!
with Ada.Streams;
procedure BDD2006 is

   Stream_Element_Size : constant := Ada.Streams.Stream_Element'Size;
   type Basic_Int is range -99 .. 99;
   Basic_Stream_Size : constant := Basic_Int'Stream_Size;
   type Larger_Int is range -9999 .. 9999;
   Larger_Stream_Size : constant := Larger_Int'Stream_Size;
   type Basic_Mod is mod 2 ** 8;
   Mod_Stream_Size : constant := Basic_Mod'Stream_Size;

   package Pack is

      type Check_Int is range -99 .. 99;
      for Check_Int'Stream_Size use Basic_Int'Stream_Size;-- ANX-C RQMT. {7;1}

      type Tst01 is range -99 .. 99;
      for Tst01'Stream_Size use Larger_Stream_Size;     -- ANX-C RQMT. {7;1}

      type Tst02 is mod 2 ** 8;
      for Tst02'Stream_Size use Mod_Stream_Size;        -- ANX-C RQMT. {7;1}

      type Tst03 is range -99 .. 99;
      for Tst03'Stream_Size use True;      -- ERROR: (A) {7;1}

      type Tst04 is range -99 .. 99;
      for Tst04'Stream_Size use 8.0;       -- ERROR: (A) {7;1}

      type Tst05 is range -99 .. 99;

      type Tst06 is range -99 .. 99;
      -- Not static:
      IVar : Integer := Basic_Stream_Size; -- POSSIBLE ERROR: [Set01] (B) {7;1}
      for Tst06'Stream_Size use IVar;      -- POSSIBLE ERROR: [Set01] (B) {7;1}

      type Tst07 is range -99 .. 99;
      for Tst07'Stream_Size use -16;       -- ERROR: (C) {7;1}

      ICnst : constant Integer := Basic_Stream_Size; -- Static.
      for Tst05'Stream_Size use ICnst;     -- OK. {7;1}

      type Tst08 is mod 2 ** 8;
      Neg_Size : constant := -Mod_Stream_Size;  -- Static.
      for Tst08'Stream_Size use Neg_Size;  -- ERROR: (C) {7;1}

      type Tst09 is range -99 .. 99;
      for Tst09'Stream_Size use 13;        -- ERROR: (D) {7;1}

      type Tst10 is mod 2 ** 8;
      Calc_Size : constant := Mod_Stream_Size-1; -- Static.
      for Tst10'Stream_Size use Calc_Size; -- ERROR: (D) {7;1}

      type Tst11 is range -9999 .. 9999;
      for Tst11'Stream_Size use Basic_Stream_Size; -- ERROR: (E) {7;1}

      type Tst12 is record
          C : Check_Int;
      end record;
      for Tst12'Stream_Size use Basic_Stream_Size; -- ERROR: (F) {7;1}

      type Tst13 is private;
      for Tst13'Stream_Size use ICnst;     -- ERROR: (F) {7;1}
         -- Note: all private types are composite. Also, 13.1(9.2/5) makes
         -- this illegal for all representation aspects.

      type Tst14 is mod 2 ** 8;
      subtype S14 is Tst14;
      for S14'Stream_Size use Mod_Stream_Size;  -- ERROR: (G) {7;1}

      type Tst15 is range -99 .. 99;
      for Tst15'Stream_Size use Tst15'Size;     -- ERROR: (H) {7;1}

      A_Size : constant := Larger_Stream_Size;
      type Tst16 is mod 2 ** 8;
      subtype Sub_Tst16 is
                Tst16 range 0 .. 50;     -- POSSIBLE ERROR: [Set02] (H) {1:7;1}
      for Tst16'Stream_Size use
                    Sub_Tst16'(A_Size);  -- POSSIBLE ERROR: [Set02] (H) {1:7;1}

      type Tst17 is range -99 .. 99;
      Obj : Tst17 := 2; -- Freezes Tst17.
      for Tst17'Stream_Size use Basic_Stream_Size; -- ERROR: (H) {7;1}

      type Tst18 is mod 2 ** 8;
      for Tst18'Stream_Size use Hidden_Size;-- ERROR: (I) {7;1}

      type Tst19 is mod 2 ** 8;
      for Tst19'Stream_Size use Mod_Size;  -- ERROR: (I) {7;1}

      Mod_Size : constant := Mod_Stream_Size; -- Static.
         -- Note: This would be legal for an aspect specification, but not
         -- an attribute definition clause.
   private
      Hidden_Size : constant := Mod_Stream_Size;

      type Tst13 is range -99 .. 99;       -- OPTIONAL ERROR: {7;1}
   end Pack;

begin
   null;
end BDD2006;
