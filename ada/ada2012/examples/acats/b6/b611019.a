-- B611019.A
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
--    Check that a renaming S1 that overrides an inherited routine S2 is
--    illegal if any of the class-wide preconditions do not fully conform.
--
--    Check that an instance is illegal if the instance contains a renaming
--    that overrides a primitive operation of a formal type where all of the
--    class-wide preconditions do not fully conform.
--
-- TEST DESCRIPTION:
--    Taken literally, no two non-trivial independently declared
--    preecondition eexpressions could ever fully conform. (To fully conform,
--    every name has to denote the same declaration, so any precondition
--    expression that references a parameter does not conform with any other
--    [as the other expression necessarily references a different parameter
--    declaration]). Therefore, we don't try any cases where one might
--    expect the expressions to conform; only cases where the precondition
--    expressions are obviously different.
--
-- CHANGE HISTORY:
--     18 Nov 2016   RLB   Created test.
--     22 Nov 2016   RLB   Corrections: The private declarations in Gen2 never
--                         override anything by 12.3(18), so instances of it
--                         are never subject to this rule. Added Gen4 to check
--                         that this check is made on generic derived types.
--
--!
package B611019 is

   type Con1 is tagged record
      Cnt : Natural;
   end record;

   function Is_Soft (A_Con : Con1) return Boolean;

   function Get_Count (A_Con : Con1) return Natural
      with Pre'Class => Is_Soft (A_Con);

   procedure Set_Count (A_Con : Con1; N : Natural)
      with Pre'Class => Is_Soft (A_Con);

   type Con2 is tagged record
      Cnt : Natural;
   end record;

   function Is_Soft (A_Con : Con2) return Boolean;

   function Get_Count (A_Con : Con2) return Natural;

   procedure Set_Count (A_Con : Con2; N : Natural);

   type Con3 is tagged record
      Cnt : Natural;
   end record;

   function Is_Soft (A_Con : Con3) return Boolean;

   function Fuzzy (A_Con : Con3) return Natural
      with Pre'Class => Is_Soft (A_Con);


   package Nest1 is
      type T01 is new Con1 with null record;

      function Is_Brown (Obj : T01) return Boolean;

      function Present (Obj : T01) return Natural
         with Pre'Class => Is_Brown (Obj);

      overriding
      function Get_Count (Obj : T01) return Natural
         renames Present;                                    -- ERROR: {1:7;1}

   end Nest1;

   package Nest2 is
      type T02 is new Con1 with private;

      function Present (Obj : T02) return Natural;

      overriding
      function Get_Count (Obj : T02) return Natural
         renames Present;                                    -- ERROR: {1:7;1}
         -- Note: Present has a class-wide precondition of True, but
         -- Get_Count has a class-wide precondition of Is_Young.
   private
      type T02 is new Con1 with null record;
   end Nest2;

   package Nest3 is
      type T03 is new Con2 with null record;

      function Has_Curves (Obj : T03) return Boolean;

      function Present (Obj : T03) return Natural
         with Pre'Class => Has_Curves (Obj);

      overriding
      function Get_Count (Obj : T03) return Natural
         renames Present;                                    -- ERROR: {1:7;1}
         -- Note: Present has a class-wide precondition of Has_Curves, but
         -- Get_Count has a class-wide precondition of True.
   end Nest3;

   generic
      type Parent is tagged private;
   package Gen1 is
      type Der is new Parent with null record;

      function Is_Round (Obj : Der) return Boolean;

      function Available (Obj : Der) return Natural
         with Pre'Class => Is_Round (Obj);

      function Get_Count (Obj : Der) return Natural renames Available;
         -- Possibly subject to the 6.1.1(17/3) rule in an instance.
   end Gen1;

   generic
      type Parent is tagged private;
   package Gen2 is
      type Der is new Parent with private;

      function Is_Round (Obj : Der) return Boolean;

   private
      type Der is new Parent with null record;

      function Available (Obj : Der) return Natural
         with Pre'Class => Is_Round (Obj);

      function Get_Count (Obj : Der) return Natural renames Available;
         -- Never subject to the 6.1.1(17/3) rule in an instance, as it
         -- never overrides a primitive of the actual by 12.3(18).
   end Gen2;

   package Inst1 is new Gen1 (Con1);                         -- ERROR: {4;1}

   package Inst2 is new Gen2 (Con1);                         -- OK. {4;1}

   package Inst3 is new Gen1 (Con2);                         -- ERROR: {4;1}

   package Inst4 is new Gen2 (Con2);                         -- OK. {4;1}

   package Inst5 is new Gen1 (Con3);                         -- OK. {4;1}

   package Inst6 is new Gen2 (Con3);                         -- OK. {4;1}

   -- Note: 12.3(18) means that there is never overriding in the private part
   -- of an instance that didn't occur in the generic, so the recheck cannot
   -- fail and we cannot test 6.1.1(17.2/4) for this rule.

   generic
      type Der is new Con1 with private;
   package Gen4 is
      type T04 is new Con1 with private;

      function Present (Obj : T04) return Natural;

      overriding
      function Get_Count (Obj : T04) return Natural
         renames Present;                                    -- ERROR: {1:7;1}
         -- Note: Present has a class-wide precondition of True, but
         -- Get_Count has a class-wide precondition of Is_Young.
   private
      type T04 is new Con1 with null record;
   end Gen4;

   generic
      type Der is new Con1 with private;
   package Gen5 is
      type T05 is new Con1 with private;

   private
      type T05 is new Con1 with null record;

      function Present (Obj : T05) return Natural;

      overriding
      function Get_Count (Obj : T05) return Natural
         renames Present;                                    -- ERROR: {1:7;1}
         -- Note: Present has a class-wide precondition of True, but
         -- Get_Count has a class-wide precondition of Is_Young.
   end Gen5;

end B611019;

