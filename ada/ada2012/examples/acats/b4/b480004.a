-- B480004.A
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
--     Check that an allocator for an access type with Storage_Size statically
--     set to zero is illegal.
--
-- CHANGE HISTORY:
--     15 Mar 2018   RLB   Created test.
--!
with Report;
procedure B480004 is

   type Acc0 is access Natural;
   for Acc0'Storage_Size use 0;
   Ptr0 : Acc0 := new Natural;                                -- ERROR: {19;1}

   type Acc1 is access all Natural;
   for Acc1'Storage_Size use 0;
   Ptr1 : Acc1 := new Natural;                                -- ERROR: {19;1}
   Var1 : aliased Natural;

   type Acc10 is access Natural;
   for Acc10'Storage_Size use 10; -- Should round up if necessary.
   Ptr10 : Acc10 := new Natural;                              -- OK. {21;1}

   type AccDyn is access Natural;
   for AccDyn'Storage_Size use Report.Ident_Int(0); -- Not static.
   PtrDyn : AccDyn := new Natural;                            -- OK. {23;1}

   generic
   package Gen is
      Ptr0 : Acc0 := new Natural'(87);                        -- ERROR: {22;1}
      Ptr10 : Acc10 := new Natural;                           -- OK. {24;1}
      procedure Dummy;
   private
      Ptr1 : Acc1 := new Natural;                             -- ERROR: {22;1}
      PtrDyn : AccDyn := new Natural;                         -- OK. {26;1}
   end Gen;

   package body Gen is
      Ptr2 : Acc0 := new Natural;                             -- ERROR: {22;1}
      Ptr3 : Acc1 := new Natural'(12);                        -- ERROR: {22;1}
      procedure Dummy is
      begin
         null;
      end Dummy;
   end Gen;

   procedure Sink0 (P : in Acc0) is
   begin
      null;
   end Sink0;

   procedure Sink1 (P : in Acc1) is
   begin
      null;
   end Sink1;

   procedure Sink10 (P : in Acc10) is
   begin
      null;
   end Sink10;

   procedure SinkDyn (P : in AccDyn) is
   begin
      null;
   end SinkDyn;

   generic
      type GAcc is access Natural;
   package GP is
      PtrGen : GAcc := new Natural;
   end GP;

   generic
      type GAcc is access Natural;
   package GPP is
      PtrVis : GAcc := null;
   private
      PtrHide : GAcc := new Natural;
   end GPP;

   -- Check rechecking in generic instantiations:
   package Inst0 is new GP (Acc0);                             -- ERROR: {4;1}
   package Inst1 is new GPP (Acc0);                            -- ERROR: {4;1}
   package Inst2 is new GP (Acc1);                             -- ERROR: {4;1}
   package Inst3 is new GPP (Acc1);                            -- ERROR: {4;1}
   package Inst4 is new GP (Acc10);                            -- OK. {4;1}
   package Inst5 is new GPP (Acc10);                           -- OK. {4;1}
   package Inst6 is new GP (AccDyn);                           -- OK. {4;1}
   package Inst7 is new GPP (AccDyn);                          -- OK. {4;1}

begin
   Ptr0 := new Natural;                                        -- ERROR: {12;1}
   Ptr0 := new Natural'(2);                                    -- ERROR: {12;1}
   Ptr0 := null;                                               -- OK. {12;1}
   Ptr1 := new Natural;                                        -- ERROR: {12;1}
   Ptr1 := new Natural'(3);                                    -- ERROR: {12;1}
   Ptr1 := Var1'Access;                                        -- OK. {12;1}
   Ptr10 := new Natural;                                       -- OK. {13;1}
   Ptr10 := new Natural'(4);                                   -- OK. {13;1}
   PtrDyn := new Natural;                                      -- OK. {14;1}
   PtrDyn := new Natural'(5);                                  -- OK. {14;1}
   Sink0 (new Natural);                                        -- ERROR: {11;2}
   Sink1 (new Natural);                                        -- ERROR: {11;2}
   Sink10 (new Natural);                                       -- OK. {12;2}
   SinkDyn (new Natural);                                      -- OK. {13;2}
   Sink0 (Ptr0);                                               -- OK. {11;2}
end B480004;
