-- B3A0005.A
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
--
-- OBJECTIVE:
--
--     Check that a dereference of an anonymous access-to-constant type
--     is a constant.
--
-- TEST DESCRIPTION:
--
--     We try assigning into anonymous access-to-constant types in all
--     of the contexts where anonymous access types are allowed. As a
--     side-effect, this test also checks that "constant" is allowed
--     in anonymous access-to-object types in all of the supported contexts.
--
--     We try various kinds of assignment, inspired by the existing test
--     B3A0003 for named access-to-constant types. In particular, we try
--     explicit dereferences, and implicit dereferences caused by both
--     record selection and array indexing. We also check that a dereference
--     of an access-to-variable in the designated object of the
--     access-to-constant works.
--
-- CHANGE HISTORY:
--      24 Apr 2008   RLB   Created test.
--
--!

procedure B3A0005 is

   type Int_Component is range 0 .. 10;

   type Array_Component is array (1 .. 4) of Int_Component;

   Inner_Arr_Obj : aliased Array_Component := (4, 66, 92, 15);

   type Record_Designated is record
      Item : Int_Component := 0;
      Arr  : Array_Component := (others => 0);
      Ptr  : access Array_Component := Inner_Arr_Obj'Access;
   end record;

   type Array_Designated is array (1 .. 3) of Record_Designated;

   Arr_Obj : aliased Array_Designated;
   Rec_Obj : aliased Record_Designated;

begin
   -- Anonymous access discriminants:
   declare
       type Test1 (D1 : access constant Array_Designated;
                   D2 : access constant Record_Designated) is record
           Item : Int_Component := 1;
       end record;
       Obj : Test1 (D1 => Arr_Obj'Access, D2 => Rec_Obj'Access);
   begin
       Obj.Item := 10;                                 -- OK.
       -- Explicit dereferences:
       Obj.D2.all := (1, (1,2,3,4), Ptr => <>);        -- ERROR:
       Obj.D2.all.Item := 10;                          -- ERROR:
       Obj.D2.all.Arr(2) := 10;                        -- ERROR:
       Obj.D2.all.Ptr.all(4) := 10;                    -- OK.
       Obj.D1.all(1).Item := 10;                       -- ERROR:
       Obj.D1.all(1).Arr(2) := 10;                     -- ERROR:
       -- Implicit dereferences:
       Obj.D2.Item := 10;                              -- ERROR:
       Obj.D2.Arr(2) := 10;                            -- ERROR:
       Obj.D2.Ptr(4) := 10;                            -- OK.
       Obj.D1(1).Item := 10;                           -- ERROR:
       Obj.D1(1).Arr(2) := 10;                         -- ERROR:
   end;

   -- Anonymous access parameters:
   declare
       procedure Test2 (P1 : access constant Array_Designated;
                        P2 : access constant Record_Designated) is
       begin
           -- Explicit dereferences:
           P2.all := (1, (1,2,3,4), Ptr => <>);        -- ERROR:
           P2.all.Item := 10;                          -- ERROR:
           P2.all.Arr(2) := 10;                        -- ERROR:
           P2.all.Ptr.all(4) := 10;                    -- OK.
           P1.all(1).Item := 10;                       -- ERROR:
           P1.all(1).Arr(2) := 10;                     -- ERROR:
           -- Implicit dereferences:
           P2.Item := 10;                              -- ERROR:
           P2.Arr(2) := 10;                            -- ERROR:
           P2.Ptr(4) := 10;                            -- OK.
           P1(1).Item := 10;                           -- ERROR:
           P1(1).Arr(2) := 10;                         -- ERROR:
       end Test2;
   begin
       Test2 (P1 => Arr_Obj'Access, P2 => Rec_Obj'Access);
   end;

   -- Anonymous access record components:
   declare
       type Test3 is record
           C1 : access constant Array_Designated := Arr_Obj'Access;
           C2 : access constant Record_Designated := Rec_Obj'Access;
           Item : Int_Component := 1;
       end record;
       Obj : Test3;
   begin
       Obj.Item := 10;                                 -- OK.
       -- Explicit dereferences:
       Obj.C2.all := (1, (1,2,3,4), Ptr => <>);        -- ERROR:
       Obj.C2.all.Item := 10;                          -- ERROR:
       Obj.C2.all.Arr(2) := 10;                        -- ERROR:
       Obj.C2.all.Ptr.all(4) := 10;                    -- OK.
       Obj.C1.all(1).Item := 10;                       -- ERROR:
       Obj.C1.all(1).Arr(2) := 10;                     -- ERROR:
       -- Implicit dereferences:
       Obj.C2.Item := 10;                              -- ERROR:
       Obj.C2.Arr(2) := 10;                            -- ERROR:
       Obj.C2.Ptr(4) := 10;                            -- OK.
       Obj.C1(1).Item := 10;                           -- ERROR:
       Obj.C1(1).Arr(2) := 10;                         -- ERROR:
   end;

   -- Anonymous access array components:
   declare
       type Test4A is array (1 .. 10) of access constant Array_Designated;
       type Test4B is array (1 .. 10) of access constant Record_Designated;
       ObjA : Test4A := (others => Arr_Obj'Access);
       ObjB : Test4B := (others => Rec_Obj'Access);
   begin
       -- Explicit dereferences:
       ObjB(6).all := (1, (1,2,3,4), Ptr => <>);       -- ERROR:
       ObjB(6).all.Item := 10;                         -- ERROR:
       ObjB(6).all.Arr(2) := 10;                       -- ERROR:
       ObjB(6).all.Ptr.all(4) := 10;                   -- OK.
       ObjA(5).all(1).Item := 10;                      -- ERROR:
       ObjA(5).all(1).Arr(2) := 10;                    -- ERROR:
       -- Implicit dereferences:
       ObjB(6).Item := 10;                             -- ERROR:
       ObjB(6).Arr(2) := 10;                           -- ERROR:
       ObjB(6).Ptr(4) := 10;                           -- OK.
       ObjA(5)(1).Item := 10;                          -- ERROR:
       ObjA(5)(1).Arr(2) := 10;                        -- ERROR:
   end;

   -- Anonymous access protected type components:
   declare
       protected Test5 is
           procedure Check;
       private
           C1 : access constant Array_Designated := Arr_Obj'Access;
           C2 : access constant Record_Designated := Rec_Obj'Access;
           Item : Int_Component := 1;
       end Test5;

       protected body Test5 is
           procedure Check is
           begin
               Item := 10;                             -- OK.
               -- Explicit dereferences:
               C2.all := (1, (1,2,3,4), Ptr => <>);    -- ERROR:
               C2.all.Item := 10;                      -- ERROR:
               C2.all.Arr(2) := 10;                    -- ERROR:
               C2.all.Ptr.all(4) := 10;                -- OK.
               C1.all(1).Item := 10;                   -- ERROR:
               C1.all(1).Arr(2) := 10;                 -- ERROR:
               -- Implicit dereferences:
               C2.Item := 10;                          -- ERROR:
               C2.Arr(2) := 10;                        -- ERROR:
               C2.Ptr(4) := 10;                        -- OK.
               C1(1).Item := 10;                       -- ERROR:
               C1(1).Arr(2) := 10;                     -- ERROR:
           end Check;
       end Test5;
   begin
       Test5.Check;
   end;

   -- Anonymous access return subtypes (in functions and extended returns):
   declare
       function Test6a return access constant Array_Designated is
       begin
           return Obj : access constant Array_Designated := Arr_Obj'Access do
               -- Explicit dereferences:
               Obj.all(1).Item := 10;                  -- ERROR:
               Obj.all(1).Arr(2) := 10;                -- ERROR:
               -- Implicit dereferences:
               Obj(1).Item := 10;                      -- ERROR:
               Obj(1).Arr(2) := 10;                    -- ERROR:
           end return;
       end Test6a;

       function Test6b return access constant Record_Designated is
       begin
           return Obj : access constant Record_Designated := Rec_Obj'Access do
               -- Explicit dereferences:
               Obj.all := (1, (1,2,3,4), Ptr => <>);   -- ERROR:
               Obj.all.Item := 10;                     -- ERROR:
               Obj.all.Arr(2) := 10;                   -- ERROR:
               Obj.all.Ptr.all(4) := 10;               -- OK.
               -- Implicit dereferences:
               Obj.Item := 10;                         -- ERROR:
               Obj.Arr(2) := 10;                       -- ERROR:
               Obj.Ptr(4) := 10;                       -- OK.
           end return;
       end Test6b;
   begin
       Test6b.all := (1, (1,2,3,4), Ptr => <>);        -- ERROR:
       Test6b.all.Item := 10;                          -- ERROR:
       Test6b.all.Arr(2) := 10;                        -- ERROR:
       Test6b.all.Ptr.all(4) := 10;                    -- OK.
       Test6a.all(1).Item := 10;                       -- ERROR:
       Test6a.all(1).Arr(2) := 10;                     -- ERROR:
       -- Implicit dereferences:
       Test6b.Item := 10;                              -- ERROR:
       Test6b.Arr(2) := 10;                            -- ERROR:
       Test6b.Ptr(4) := 10;                            -- OK.
       Test6a(1).Item := 10;                           -- ERROR:
       Test6a(1).Arr(2) := 10;                         -- ERROR:
   end;

   -- Anonymous access object declarations, including renamed objects:
   declare
       ObjA : access constant Array_Designated  := Arr_Obj'Access;
       ObjB : access constant Record_Designated := Rec_Obj'Access;
       RenObjA : access constant Array_Designated renames ObjA;
       RenObjB : access constant Record_Designated renames ObjB;
   begin
       -- Explicit dereferences:
       ObjB.all := (1, (1,2,3,4), Ptr => <>);          -- ERROR:
       ObjB.all := (1, (1,2,3,4));                     -- ERROR:
       ObjB.all.Item := 10;                            -- ERROR:
       ObjB.all.Arr(2) := 10;                          -- ERROR:
       ObjB.all.Ptr.all(4) := 10;                      -- OK.
       ObjA.all(1).Item := 10;                         -- ERROR:
       ObjA.all(1).Arr(2) := 10;                       -- ERROR:
       RenObjB.all := (1, (1,2,3,4), Ptr => <>);       -- ERROR:
       RenObjB.all.Item := 10;                         -- ERROR:
       RenObjB.all.Arr(2) := 10;                       -- ERROR:
       RenObjB.all.Ptr.all(4) := 10;                   -- OK.
       RenObjA.all(1).Item := 10;                      -- ERROR:
       RenObjA.all(1).Arr(2) := 10;                    -- ERROR:
       -- Implicit dereferences:
       ObjB.Item := 10;                                -- ERROR:
       ObjB.Arr(2) := 10;                              -- ERROR:
       ObjB.Ptr(4) := 10;                              -- OK.
       ObjA(1).Item := 10;                             -- ERROR:
       ObjA(1).Arr(2) := 10;                           -- ERROR:
       RenObjB.Item := 10;                             -- ERROR:
       RenObjB.Arr(2) := 10;                           -- ERROR:
       RenObjB.Ptr(4) := 10;                           -- OK.
       RenObjA(1).Item := 10;                          -- ERROR:
       RenObjA(1).Arr(2) := 10;                        -- ERROR:
   end;

   -- Anonymous access formal objects:
   declare
       generic
           O1 : access constant Array_Designated;
           O2 : access constant Record_Designated;
       procedure Test8;

       procedure Test8 is
       begin
           -- Explicit dereferences:
           O2.all := (1, (1,2,3,4), Ptr => <>);    -- ERROR:
           O2.all.Item := 10;                      -- ERROR:
           O2.all.Arr(2) := 10;                    -- ERROR:
           O2.all.Ptr.all(4) := 10;                -- OK.
           O1.all(1).Item := 10;                   -- ERROR:
           O1.all(1).Arr(2) := 10;                 -- ERROR:
           -- Implicit dereferences:
           O2.Item := 10;                          -- ERROR:
           O2.Arr(2) := 10;                        -- ERROR:
           O2.Ptr(4) := 10;                        -- OK.
           O1(1).Item := 10;                       -- ERROR:
           O1(1).Arr(2) := 10;                     -- ERROR:
       end Test8;

       procedure Do_It is new Test8 (Arr_Obj'Access, Rec_Obj'Access);
   begin
       Do_It;
   end;
end B3A0005;
