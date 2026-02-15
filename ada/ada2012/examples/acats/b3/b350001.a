-- B350001.A
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
--     Check that Legality Rules are enforced when Default_Value is specified.
--
-- TEST DESCRIPTION:
--     The Legality Rules checked herein are:
--        (A) The expression for the Default_Value aspect shall be the
--            type of the declaration on which it appears - 3.5(56.5/3).
--        (B) The expression for the Default_Value aspect shall be static -
--            3.5(56.3/3).
--        (C) The expression for the Default_Value aspect shall be explicit -
--            3.5(56.3/3) [only relevant for boolean types].
--        (D) The Default_Value aspect shall be specified on a scalar
--            type - 3.5(56.2/3).
--        (E) The Default_Value aspect shall be specified on a
--            full_type_declaration - 3.5(56.3/3).
--        (F) The expression for the Default_Value aspect cannot freeze the
--            type itself (directly or indirectly) - 13.1(9.1/5), as added by
--            Binding Interpretation AI12-0181-1. (This prevents
--            using attributes, objects, conversions, or qualifications of the
--            type or of a subtype of it.) Note that there is an exception
--            for enumeration literals added by AI12-0367-1 in 13.14(10/5);
--            as this AI is a binding interpretation and not allowing literals
--            would be nonsense, we assume that AI is supported.
--        (G) A derived type that inherits primitive subprograms cannot specify
--            a different value of aspect Default_Value - 13.1(10/4).
--            Note: This rule will be repealed in Ada 202x by AI12-0376-1, so
--            we no longer test it.
--        (H) The expression for the Default_Value aspect of a type given in
--            the visible part of a package cannot name a declaration given
--            in the private part - 13.1.1(11/3).
--        (I) The expression for the Default_Value aspect of a type cannot name
--            a declaration that comes after the freezing point of the type
--            13.1.1(13/3).
--        (J) The expression for the Default_Value aspect of a type must
--            resolve to the same entities at the first freezing point as at
--            the end of the declaration list - 13.1.1(13/3).
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
-- CHANGE HISTORY:
--     19 Mar 20   RLB     Created test from existing BDD2007 for Stream_Size.
--     14 May 20   RLB     Removed (G) test cases as the rule will be repealed
--                         by AI12-0376-1.
--
--!
procedure B350001 is

   Outer_Default_Value : constant := 10;

   package Pack is

      type Tst01 is range -99 .. 99
         with Default_Value => True;     -- ERROR: (A) {1:7;1}

      type Tst02 is mod 2 ** 8
         with Default_Value => 8.0;      -- ERROR: (A) {1:7;1}

      type Tst03 is delta 0.25 range -4.0 .. 4.0
         with Default_Value => 0;        -- ERROR: (A) {1:7;1}

      type Tst04 is (Unknown, Red, Green, Blue)
         with Default_Value => 0;        -- ERROR: (A) {1:7;1}

      type Tst05 is range -99 .. 99
         with Default_Value => Cnst;     -- OK. {1:7;1}

      type Tst06 is range -99 .. 99
         with Default_Value => Var;      -- POSSIBLE ERROR: [Set01] (B) {1:7;1}
         -- Note that this value also has freezing issues. It's not possible to
         -- have a non-static value without triggering those.

      type Tst07 is new Boolean
         with Default_Value;             -- ERROR: (C) {1:7;1}

      type Tst08 is new Boolean
         with Default_Value => True;     -- OK. {1:7;1}

      type Tst09 is access Integer
         with Default_Value => null;     -- ERROR: (D) {1:7;1}

      type Tst10 is digits 3
         with Default_Value => 0.0;      -- OK. {1:7;1}

      type Tst11 is array (1..3) of Integer
         with Default_Value => (1,2,3);  -- ERROR: (D) {1:7;1}

      type Tst12 is record
         X, Y : Integer;
      end record
         with Default_Value => (0,0);    -- ERROR: (D) {3:7;1}

      type Tst13 is private
         with Default_Value => 0;        -- ERROR: (E) {1:7;1}
         -- Additionally, 13.1(9.2/5) makes this illegal for all
         -- representation aspects.

      type Tst14 is mod 2 ** 8;
      subtype S14 is Tst14
         with Default_Value => 0;        -- ERROR: (E) {1:7;1}

      type Tst15 is range -99 .. 99
         with Default_Value => Tst15'Last; -- ERROR: (F) {1:7;1}

      A_Val : constant := 100;
      type Tst16 is mod 2 ** 8
         with Default_Value =>
                    Sub_Tst16'(A_Val);   -- POSSIBLE ERROR: [Set02] (F) {2:7;1}
      subtype Sub_Tst16 is
                Tst16 range 0 .. 50;     -- POSSIBLE ERROR: [Set02] (F) {1:7;1}

      type Tst17 is (Unknown, Raw, Bound, Solved)
         with Default_Value => Tst17'Val(0); -- ERROR: (F) {1:7;1}

      type Tst18 is range 1 .. 5
         with Default_Value => Max;      -- POSSIBLE ERROR: [Set03] (F) {1:7;1}
      Max : constant Tst18 := 5;         -- POSSIBLE ERROR: [Set03] (F) {7;1}
         -- Max freezes Tst18 which freezes Max which ...

      type Tst19 is (Unknown, Raw, Bound, Solved)
         with Default_Value => Not_Set;  -- POSSIBLE ERROR: [Set04] (F) {1:7;1}
      Not_Set : constant Tst19 :=
                               Unknown;  -- POSSIBLE ERROR: [Set04] (F) {1:7;1}

      --type Tst20 is mod 2**8
      --   with Default_Value => 0;        -- OK. {1:7;1}
      --function Is_Primitive (A : Tst20) return Boolean is (True);

      --type Tst21 is new Tst20
      --   with Default_Value => 10;       -- ERROR: (G) {1:7;1}
      --   This will be legal in Ada 202x, so we don't test it nor
      --   require it to work for Ada 2012.
      
      --type Tst22 is (Unknown, Raw, Bound, Solved)
      --   with Default_Value => Unknown;  -- OK. {1:7;1}
      --function Bad_Value return Tst22 is (Unknown);

      --type Tst23 is new Tst22
      --   with Default_Value => Solved;   -- ERROR: (G) {1:7;1}
      --   This will be legal in Ada 202x, so we don't test it nor
      --   require it to work for Ada 2012.

      type Tst24 is mod 2 ** 8
         with Default_Value =>
                            Hidden_Value;-- POSSIBLE ERROR: [Set05] (H) {2:7;1}

      type Tst25 is range -99 .. 99
         with Default_Value => Sml_Value;-- POSSIBLE ERROR: [Set06] (I) {1:7;1}
      Obj : Tst25 := 2; -- Freezes Tst25 -- POSSIBLE ERROR: [Set06] (I) {7;1}

      type Tst26 is range -99 .. 99 with
         Default_Value =>
                     Outer_Default_Value;-- POSSIBLE ERROR: [Set07] (J) {2:7;1}

      -- Static:
      Cnst : constant := 0;

      Sml_Value : constant := 12;        -- POSSIBLE ERROR: [Set06] (I) {7;1}

      Obj26 : Tst26 := 2; -- Freezes Tst26,
                          -- Default_Value = B350001.Outer_Default_Value.
                                         -- POSSIBLE ERROR: [Set07] (J) {2:7;1}
      Outer_Default_Value : constant
                               := 5;     -- POSSIBLE ERROR: [Set07] (J) {1:7;1}
                       -- Different declaration of Outer_Default_Value.

      -- Not static:
      Var : Tst06 := 10;                 -- POSSIBLE ERROR: [Set01] (B) {7;1}

      -- Resolution recheck here.
   private                               -- POSSIBLE ERROR: [Set07] (J) {1:7;0}
      Hidden_Value : constant := 99;     -- POSSIBLE ERROR: [Set05] (H) {7;1}

      type Tst13 is range -99 .. 99;     -- OPTIONAL ERROR: {7;1}

   end Pack;

begin
   null;
end B350001;
