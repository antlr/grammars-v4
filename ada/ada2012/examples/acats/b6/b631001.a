-- B631001.A
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
--
--     Check that a prefixed view is intrinsic.
--
-- TEST DESCRIPTION:
--
--     We check that an access-to-subprogram value cannot be created from
--     a prefixed view, as 3.10.2(32/5) does not allow 'Access to be applied
--     to an intrinsic subprogram.
--
-- CHANGE HISTORY:
--     27 Sep 19   RLB     Created test.
--
--!
procedure B631001 is

   package Pack1 is
      type T is tagged record
         C : Natural;
      end record;

      procedure Bump (Obj : in out T);

      procedure Add (Obj : in out T; Val : in Natural);

      function Get (Obj : in T) return Natural;
   end Pack1;

   package Pack2 is
      An_Obj : Pack1.T;

      -- Renamings (note that these retain the Intrinsic convention):

      procedure A_Bump renames An_Obj.Bump;                -- OK. {7;1}

      procedure An_Add (Val : in Natural)
         renames An_Obj.Add;                               -- OK. {1:7;1}

      function A_Get return Natural renames An_Obj.Get;    -- OK. {7;1}

      -- Access types:

      type Acc_Bump is access procedure;

      type Acc_Add is access procedure (Val : in Natural);

      type Acc_Get is access function return Natural;

      -- Subprograms:

      function OK_Get return Natural;

      function Read1 (F : in Acc_Get) return Natural;

      function Read2 (F : access function return Natural) return Natural;

      -- Tests:

      V1 : Acc_Bump := An_Obj.Bump'Access;                 -- ERROR: {7;1}

      V2 : Acc_Bump := A_Bump'Access;                      -- ERROR: {7;1}

      V3 : Acc_Add := An_Obj.Add'Access;                   -- ERROR: {7;1}

      V4 : Acc_Add := An_Add'Access;                       -- ERROR: {7;1}

      V5 : Acc_Get := An_Obj.Get'Access;                   -- ERROR: {7;1}

      V6 : Acc_Get := A_Get'Access;                        -- ERROR: {7;1}

      V7 : Acc_Get := OK_Get'Access;                       -- OK. {7;1}

      V11 : Natural := Read1 (An_Obj.Get'Access);          -- ERROR: {7;1}

      V12 : Natural := Read1 (A_Get'Access);               -- ERROR: {7;1}

      V13 : Natural := Read1 (OK_Get'Access);              -- OK. {7;1}

      V14 : Natural := Read2 (An_Obj.Get'Access);          -- ERROR: {7;1}

      V15 : Natural := Read2 (A_Get'Access);               -- ERROR: {7;1}

      V16 : Natural := Read2 (OK_Get'Access);              -- OK. {7;1}

   end Pack2;

   package body Pack1 is

      procedure Bump (Obj : in out T) is
      begin
         Obj.C := Obj.C + 1;
      end Bump;


      procedure Add (Obj : in out T; Val : in Natural) is
      begin
         Obj.C := Obj.C + Val;
      end Add;


      function Get (Obj : in T) return Natural is
      begin
          return Obj.C;
      end Get;

   end Pack1;

   package body Pack2 is

      function OK_Get return Natural is
      begin
         return 0;
      end OK_Get;

      function Read1 (F : in Acc_Get) return Natural is
      begin
         return F.all;
      end Read1;


      function Read2 (F : access function return Natural) return Natural is
      begin
         return F.all;
      end Read2;

   end Pack2;

begin
   Pack2.An_Obj.Bump;                                      -- OK. {4;1}
   Pack2.An_Obj.Add (4);                                   -- OK. {4;1}
end B631001;
