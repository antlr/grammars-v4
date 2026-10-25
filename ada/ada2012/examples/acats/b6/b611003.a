-- B611003.A
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
--
--     Check that Pre'Class or Post'Class cannot be specified for a generic
--     subprogram.
--
--     Check that Pre'Class or Post'Class cannot be specified for a subprogram
--     that is not a primitive subprogram of some tagged type.
--
--     Check that Pre'Class or Post'Class cannot be specified for packages,
--     objects, or types.
--
-- TEST DESCRIPTION:
--     These aspects are required by 13.1.1(16/3) to be given on a primitive
--     subprogram of a tagged type. A generic subprogram can never be
--     primitive (remember that a generic subprogram is not a subprogram).
--     Cases involving tasks and protected objects are tested separately.
--
-- CHANGE HISTORY:
--     04 Feb 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
package B611003 is

   Glob : Integer := 0;

   generic
      type Gen_Tagged is tagged limited private;
   procedure Gen (Obj : in out Gen_Tagged);

   type Intf is limited interface;

   function Is_OK (Obj : in Intf) return Boolean is abstract;

   procedure Proc1 (Obj : in out Intf) is null
                             with Pre'Class => Is_OK (Obj);    -- OK. {35;1}

   procedure Proc2 (Obj : in out Intf) is null
                             with Post'Class => Is_OK (Obj);   -- OK. {35;1}

   type Root is tagged record
       C : Character;
   end record;

   function Is_Old (Obj : in Root) return Boolean;

   procedure Proc3 (Obj : in out Root)
                             with Pre'Class => Is_Old (Obj);   -- OK. {35;1}

   procedure Proc4 (Obj : in out Root)
                             with Post'Class => Is_Old (Obj);  -- OK. {35;1}

   ---- First objective:

   generic
   procedure GProc1 (Obj : in out Intf)
                             with Pre'Class => Is_OK (Obj);    -- ERROR: {35;1}

   generic
   procedure GProc2 (Obj : in out Intf)
                             with Post'Class => Is_OK (Obj);   -- ERROR: {35;1}

   generic
   procedure GProc3 (Obj : in out Root)
                             with Pre'Class => Is_Old (Obj);   -- ERROR: {35;1}

   generic
   procedure GProc4 (Obj : in out Root)
                             with Post'Class => Is_Old (Obj);  -- ERROR: {35;1}

   ---- Second objective:

   procedure Proc5 (Obj : in out Natural)
                             with Pre'Class => Obj mod 2 = 0;  -- ERROR: {35;1}

   procedure Proc6 (Obj : in out Natural)
                             with Post'Class => Obj mod 2 = 0; -- ERROR: {35;1}

   package Nest is
      procedure NProc1 (Obj : in out Intf) is null
                             with Pre'Class => Is_OK (Obj);    -- ERROR: {35;1}

      procedure NProc2 (Obj : in out Intf) is null
                             with Post'Class => Is_OK (Obj);   -- ERROR: {35;1}

      procedure NProc3 (Obj : in out Root) is null
                             with Pre'Class => Is_Old (Obj);   -- ERROR: {35;1}

      procedure NProc4 (Obj : in out Root) is null
                             with Post'Class => Is_Old (Obj);  -- ERROR: {35;1}

      procedure IProc1 is new Gen (Root)
                             with Pre'Class => Is_OK (Obj);    -- ERROR: {35;1}

      procedure IProc2 is new Gen (Root)
                             with Post'Class => Is_OK (Obj);   -- ERROR: {35;1}

      procedure IProc3 is new Gen (Root)
                             with Pre'Class => Is_Old (Obj);   -- ERROR: {35;1}

      procedure IProc4 is new Gen (Root)
                             with Post'Class => Is_Old (Obj);  -- ERROR: {35;1}

   end Nest;

   procedure CProc1 (Obj : in out Intf'Class)
                             with Pre'Class => Is_OK (Obj);    -- ERROR: {35;1}

   procedure CProc2 (Obj : in out Intf'Class)
                             with Post'Class => Is_OK (Obj);   -- ERROR: {35;1}

   procedure CProc3 (Obj : in out Root'Class)
                             with Pre'Class => Is_Old (Obj);   -- ERROR: {35;1}

   procedure CProc4 (Obj : in out Root'Class)
                             with Post'Class => Is_Old (Obj);  -- ERROR: {35;1}

   generic
      type Gen_Tagged is tagged limited private;
   package GenP is
      -- Subprograms with parameters of Gen_Tagged are not primitive.
      function Is_OK (Obj : in Gen_Tagged) return Boolean;

      procedure Proc1 (Obj : in out Gen_Tagged)
                             with Pre'Class => Is_OK (Obj);    -- ERROR: {35;1}

      procedure Proc2 (Obj : in out Gen_Tagged)
                             with Post'Class => Is_OK (Obj);   -- ERROR: {35;1}

      -- Subprograms with parameters of New_Gen_Tagged are primitive.
      type New_Gen_Tagged is new Gen_Tagged with null record;
      function Is_OK (Obj : in New_Gen_Tagged) return Boolean;

      procedure Proc3 (Obj : in out New_Gen_Tagged)
                             with Pre'Class => Is_OK (Obj);    -- OK. {35;1}

      procedure Proc4 (Obj : in out New_Gen_Tagged)
                             with Post'Class => Is_OK (Obj);   -- OK. {35;1}

   end GenP;

   ---- Third objective:

   package Nest1 with Pre'Class => Glob > 0 is                 -- ERROR: {23;3}
      procedure P (Arg : in out Root);
   end Nest1;

   package Nest2 with Post'Class => Glob = 0 is                -- ERROR: {23;3}
      procedure P (Arg : in out Root);
   end Nest2;

   Fooey1 : Root := (C => 'A')
                       with Pre'Class => Is_Old(Fooey1);       -- ERROR: {29;1}

   Fooey2 : Root := (C => 'A')
                       with Post'Class => Is_Old(Fooey2);      -- ERROR: {29;1}

   type Rec1 is tagged record
      C : Character;
   end record with Pre'Class => Is_OK(Rec1);                   -- ERROR: {20;1}

   function Is_OK (Obj : in Rec1) return Boolean;

   type Rec2 is tagged private with Post'Class => Is_OK(Rec2); -- ERROR: {37;1}

   function Is_OK (Obj : in Rec2) return Boolean;

private

   type Rec2 is tagged null record; -- Complete this to avoid unrelated errors.

end B611003;
