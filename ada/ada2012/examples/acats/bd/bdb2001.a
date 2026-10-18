-- BDB2001.A
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
--     Check that a call of an instance of Unchecked_Deallocation is illegal
--     for an access type with Storage_Size statically set to zero.
--
-- CHANGE HISTORY:
--     15 Mar 2018   RLB   Created test.
--!
with Ada.Unchecked_Deallocation;
with Report;
procedure BDB2001 is

   type Acc0 is access Natural;
   for Acc0'Storage_Size use 0;
   Ptr0 : Acc0;
   procedure Free is
              new Ada.Unchecked_Deallocation (Natural, Acc0);  -- OK. {1:4;1}


   type Acc1 is access all Natural;
   for Acc1'Storage_Size use 0;
   Var1 : aliased Natural;
   Ptr1 : Acc1 := Var1'Access;
   procedure Free1 is
              new Ada.Unchecked_Deallocation (Natural, Acc1);  -- OK. {1:4;1}


   type Acc10 is access Natural;
   for Acc10'Storage_Size use 10; -- Should round up if necessary.
   Ptr10 : Acc10;
   procedure Free2 is
              new Ada.Unchecked_Deallocation (Natural, Acc10); -- OK. {1:4;1}

   type AccDyn is access Natural;
   for AccDyn'Storage_Size use Report.Ident_Int(0); -- Not static.
   PtrDyn : AccDyn;
   procedure Free3 is
              new Ada.Unchecked_Deallocation (Natural, AccDyn);-- OK. {1:4;1}

   generic
   package Gen is
      procedure Dummy;
   end Gen;

   package body Gen is
      procedure Dummy is
      begin
         Free (Ptr0);                                          -- ERROR: {10;1}
         Free1 (Ptr1);                                         -- ERROR: {10;1}
         Free2 (Ptr10);                                        -- OK. {10;1}
         Free3 (PtrDyn);                                       -- OK. {10;1}
      end Dummy;
   end Gen;

   generic
      type GAcc is access Natural;
   package GP is
      procedure GFree is
           new Ada.Unchecked_Deallocation (Natural, GAcc);     -- OK. {1:7;1}
      procedure Do_It (PtrGen : in out GAcc);
   end GP;

   package body GP is
      procedure Do_It (PtrGen : in out GAcc) is
      begin
          GFree (PtrGen);
      end Do_It;
   end GP;


   -- Check rechecking in generic instantiations:
   package Inst0 is new GP (Acc0);                             -- OK. {4;1}
      -- Note: This is legal, but a call to Do_It is erroneous (the
      -- advice is that it raises Program_Error.
   package Inst1 is new GP (Acc10);                            -- OK. {4;1}
   package Inst2 is new GP (AccDyn);                           -- OK. {4;1}

begin
   Free (Ptr0);                                                -- ERROR: {4;1}
   Free1 (Ptr1);                                               -- ERROR: {4;1}
   Free2 (Ptr10);                                              -- OK. {4;1}
   Free3 (PtrDyn);                                             -- OK. {4;1}
end BDB2001;
