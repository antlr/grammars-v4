-- B611018.A
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
--
--*
--
-- OBJECTIVE:
--    Check that for an abstract type T that inherits homographs of a
--    subprogram S from two different ancestors with non-conforming
--    preconditions, the inherited S cannot be called by a statically
--    bound call.
--
--    Check that for a nonabstract type T that inherits homographs of a
--    subprogram S from two different ancestors with non-conforming
--    preconditions, the inherited S is illegal if it is not overridden.
--
-- TEST DESCRIPTION:
--    We declare several interfaces and concrete types that include the
--    declaration of a function Get_Count and a procedure Set_Count, with
--    various class-wide preconditions. We then derive types from combinations
--    of the types to check the entire rule - 6.1.1(10-16/3), both the cases
--    that are allowed and those that need to be rejected.
--
--    Note that taken literally, no two non-trivial independently declared
--    preecondition could ever fully conform. (To fully conform, every name
--    has to denote the same declaration, so any precondition expression that
--    references a parameter does not conform with any other [as the other
--    references a different parameter declaration]). We only try cases
--    where the precondition expressions are obviously different in order
--    to allow incorrect but more useful implementations of this rule.
--
-- CHANGE HISTORY:
--     17 Nov 2016   RLB   Created test.
--     18 Nov 2016   RLB   Split renames objectives into a separate test.
--     30 Nov 2016   RLB   Type T08 is supposed to be abstract.
--
--!
package B611018 is

   type Intf1 is limited interface;

   function Is_Blue (A_Intf : Intf1) return Boolean is abstract;

   function Get_Count (A_Intf : Intf1) return Natural is abstract
      with Pre'Class => Is_Blue (A_Intf);

   procedure Set_Count (A_Intf : Intf1; N : Natural) is abstract
      with Pre'Class => Is_Blue (A_Intf);

   type Intf2 is limited interface;

   function Is_Soft (A_Intf : Intf2) return Boolean is abstract;

   function Get_Count (A_Intf : Intf2) return Natural is abstract
      with Pre'Class => Is_Soft (A_Intf);

   procedure Set_Count (A_Intf : Intf2; N : Natural) is null
      with Pre'Class => Is_Soft (A_Intf);

   type Abstr1 is abstract tagged record
      Cnt : Natural;
   end record;

   function Has_Curves (A_Abstr : Abstr1) return Boolean is abstract;

   function Get_Count (A_Abstr : Abstr1) return Natural is abstract
      with Pre'Class => Has_Curves (A_Abstr);

   procedure Set_Count (A_Abstr : Abstr1; N : Natural) is abstract
      with Pre'Class => Has_Curves (A_Abstr);

   type Con1 is tagged record
      Cnt : Natural;
   end record;

   function Is_Young (A_Con : Con1) return Boolean;

   function Get_Count (A_Con : Con1) return Natural
      with Pre'Class => Is_Young (A_Con);

   procedure Set_Count (A_Con : Con1; N : Natural)
      with Pre'Class => Is_Young (A_Con);

   type Con2 is tagged record
      Cnt : Natural;
   end record;

   function Is_Young (A_Con : Con2) return Boolean;

   function Get_Count (A_Con : Con2) return Natural;

   procedure Set_Count (A_Con : Con2; N : Natural);

   type Con3 is tagged record
      Cnt : Natural;
   end record;

   function Is_Young (A_Con : Con3) return Boolean;

   function Get_Count (A_Con : Con3) return Natural
      with Pre'Class => True;

   procedure Set_Count (A_Con : Con3; N : Natural)
      with Pre'Class => True;

   package Nest1 is
      type T01 is abstract new Intf2 and Intf1 with record     -- OK.
         Cnt : Natural;
      end record;
      -- For Get_Count, the primitive subprogam of Intf2 is abstract,
      -- so 6.1.1(11/3) is not satisfied and the rule does not apply.
      -- For Set_Count, the primitive subprogam of Intf2 is null,
      -- so 6.1.1(11/3) is not satisfied and the rule does not apply.
   end Nest1;

   package Nest2 is
      type T02 is abstract new Con2 and Intf1 with null record;  -- OK.
      -- For both Get_Count and Set_Count, the class-wide precondition of
      -- True does apply, so the 6.1.1(12/3) is not satisfied and the rule
      -- does not apply. Check that Get_Count is not abstract:
      function Check_It (Obj : T02'Class) return Natural is
          (Get_Count (T02(Obj)));                             -- OK. {12;2}
          -- Statically bound call of Get_Count.
   end Nest2;

   package Nest3 is
      type T03 is abstract new Con3 and Intf1 with null record;  -- OK.
      -- For both Get_Count and Set_Count, the class-wide precondition of
      -- True does apply, so the 6.1.1(12/3) is not satisfied and the rule
      -- does not apply. Check that Get_Count is not abstract:
      function Check_It (Obj : T03'Class) return Natural is
          (Get_Count (T03(Obj)));                             -- OK. {12;2}
          -- Statically bound call of Get_Count.
   end Nest3;

   package Nest4 is
      type T04 is new Con1 and Intf1 with null record;        -- OK.
      -- The rule applies, and we've overridden the routines.

      overriding
      function Is_Young (Obj : T04) return Boolean;

      overriding
      function Is_Blue (Obj : T04) return Boolean;

      overriding
      function Get_Count (Obj : T04) return Natural;

      overriding
      procedure Set_Count (Obj : T04; N : Natural);

      function Check_It (Obj : T04'Class) return Natural is
          (Get_Count (T04(Obj)));                             -- OK. {12;2}
          -- Statically bound call of Get_Count.
   end Nest4;

   package Nest5 is
      type T05 is abstract new Abstr1 and Intf1 with null record; -- OK.
      -- For both Get_Count and Set_Count, the primitive subprogam of Abstr1
      -- abstract, so 6.1.1(11/3) is not satisfied and the rule does not apply.
      -- But all of the routines are abstract anyway, so we couldn't tell the
      -- difference.
   end Nest5;

   package Nest6 is
      type T06 is new Con1 and Intf2 with record
         Foo : Positive;
      end record;                          -- POSSIBLE ERROR: [Set1] {2:7;1}
      -- The rule applies; for both Get_Count and Set_Count, different
      -- class-wide preconditions apply to the two ancestor routines.
      -- Thus, Get_Count and Set_Count require overriding. But
      -- Set_Count is missing; the error can be reported on the type or at
      -- the end of the nested package.

      overriding
      function Is_Soft (Obj : T06) return Boolean;

      overriding
      function Get_Count (Obj : T06) return Natural;

   end Nest6;                              -- POSSIBLE ERROR: [Set1] {1:1;1}

   package Nest7 is
      type T07 is new Con1 and Intf2 with record
         Foo : Positive;
      end record;                          -- POSSIBLE ERROR: [Set2] {2:7;1}
      -- The rule applies; for both Get_Count and Set_Count, different
      -- class-wide preconditions apply to the two ancestor routines.
      -- Thus, Get_Count and Set_Count require overriding. But
      -- Set_Count is missing; the error can be reported on the type or at
      -- the end of the nested package.

      overriding
      function Is_Soft (Obj : T07) return Boolean;

      overriding
      procedure Set_Count (Obj : T07; N : Natural);

   end Nest7;                              -- POSSIBLE ERROR: [Set2] {1:1;1}

   package Nest8 is
      type T08 is abstract new Con1 and Intf2 with private;
      -- The rule applies; Get_Count and Set_Count are abstract (Is_Soft and
      -- Is_Young are concrete).

      overriding
      function Is_Soft (Obj : T08) return Boolean;

      function Check_It (Obj : T08'Class) return Natural is
          (Get_Count (T08(Obj)));                             -- ERROR: {12;2}
          -- Statically bound call of Get_Count.
   private
      type T08 is abstract new Con1 and Intf2 with null record;
   end Nest8;

end B611018;

