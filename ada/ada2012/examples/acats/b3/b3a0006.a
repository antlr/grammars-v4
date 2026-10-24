-- B3A0006.A
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
--     Check that an anonymous access-to-variable type cannot designate
--     a constant.
--
-- TEST DESCRIPTION:
--
--     We try assigning access-to-constant and Cnst'Access values into
--     anonymous access-to-variable objects in all of the contexts where
--     anonymous access types are allowed.
--
-- CHANGE HISTORY:
--      24 Apr 2008   RLB   Created test.
--
--!
procedure B3A0006 is

   type Int_Component is range 0 .. 10;

   Int_Obj : aliased Int_Component := 4;
   Int_Cnst : aliased constant Int_Component := 7;

   type Designated is record
      Item : Int_Component := 0;
      Ptr  : access constant Int_Component := Int_Obj'Access;
   end record;

   type Acc_to_Cnst is access constant Designated;

   Var  : aliased Designated := (6, Ptr => <>);
   Cnst : aliased constant Designated := (4, Ptr => <>);

   AVar : access Designated := Var'Access;
   ACnst: access constant Designated := Cnst'Access;
   NCnst: Acc_to_Cnst := Cnst'Access;

begin
   -- Anonymous access discriminants:
   declare
       type Test1 (D : access Designated) is record
           Item : Int_Component := 1;
       end record;
       Obj1 : Test1 (D => Var'Access);                 -- OK.
       Obj2 : Test1 (D => Cnst'Access);                -- ERROR:
       Obj3 : Test1 (D => AVar);                       -- OK.
       Obj4 : Test1 (D => ACnst);                      -- ERROR:
       Obj5 : Test1 (D => NCnst);                      -- ERROR:
   begin
       Obj1.D.Item := 0;                               -- OK.
       Obj1.D.Ptr := Int_Cnst'Access;                  -- OK.
   end;

   -- Anonymous access parameters:
   declare
       procedure Test2 (P : access Designated) is
       begin
           P.Item := 10;                               -- OK.
           P.Ptr := Int_Cnst'Access;                   -- OK.
       end Test2;
   begin
       Test2 (P => Var'Access);                        -- OK.
       Test2 (P => Cnst'Access);                       -- ERROR:
       Test2 (P => AVar);                              -- OK.
       Test2 (P => ACnst);                             -- ERROR:
       Test2 (P => NCnst);                             -- ERROR:
   end;

   -- Anonymous access record component:
   declare
       type Test3 is record
           C: access Designated;
       end record;
       Obj1 : Test3 := (C => Var'Access);              -- OK.
       Obj2 : Test3 := (C => Cnst'Access);             -- ERROR:
       Obj3 : Test3 := (C => AVar);                    -- OK.
       Obj4 : Test3 := (C => ACnst);                   -- ERROR:
       Obj5 : Test3 := (C => NCnst);                   -- ERROR:
   begin
       Obj1.C.Item := 10;                              -- OK.
       Obj1.C.Ptr := Int_Cnst'Access;                  -- OK.
   end;

   -- Anonymous access array components:
   declare
       type Test4 is array (1 .. 10) of access Designated;
       Obj1 : Test4 := (others => Var'Access);         -- OK.
       Obj2 : Test4 := (others => Cnst'Access);        -- ERROR:
       Obj3 : Test4 := (others => AVar);               -- OK.
       Obj4 : Test4 := (others => ACnst);              -- ERROR:
       Obj5 : Test4 := (others => NCnst);              -- ERROR:
   begin
       Obj1(1).Item := 10;                             -- OK.
       Obj1(1).Ptr := Int_Cnst'Access;                 -- OK.
   end;

   -- Anonymous access protected type components:
   declare
       protected Test5 is
           procedure Check;
       private
           C : access Designated;
       end Test5;

       protected body Test5 is
           procedure Check is
           begin
               C := Var'Access;                        -- OK.
               C := Cnst'Access;                       -- ERROR:
               C := AVar;                              -- OK.
               C := ACnst;                             -- ERROR:
               C := NCnst;                             -- ERROR:
           end Check;
       end Test5;
   begin
       Test5.Check;
   end;

   -- Anonymous access return subtypes (in functions and extended returns):
   declare
       function Test6 (P : Int_Component) return access Designated is
       begin
           case P is
              when 1 =>
                  return Var'Access;                    -- OK.
              when 2 =>
                  return Cnst'Access;                   -- ERROR:
              when 3 =>
                  return AVar;                          -- OK.
              when 4 =>
                  return ACnst;                         -- ERROR:
              when 5 =>
                  return NCnst;                         -- ERROR:
              when 6 =>
                  return Obj : access Designated :=
                                          Var'Access;   -- OK.
              when 7 =>
                  return Obj : access Designated :=
                                         Cnst'Access;   -- ERROR:
              when 8 =>
                  return Obj : access Designated :=
                                         AVar;          -- OK.
              when 9 =>
                  return Obj : access Designated :=
                                         ACnst;         -- ERROR:
              when 10 =>
                  return Obj : access Designated :=
                                         NCnst;         -- ERROR:
              when others => null;
           end case;
       end Test6;

       Obj : access Designated;
   begin
       Obj := Test6(1);
   end;

   -- Anonymous access object declarations, including renamed objects:
   declare
       Obj : access Designated;
       RenObj : access Designated renames Obj;
   begin
       Obj := Var'Access;                               -- OK.
       Obj := Cnst'Access;                              -- ERROR:
       Obj := AVar;                                     -- OK.
       Obj := ACnst;                                    -- ERROR:
       Obj := NCnst;                                    -- ERROR:
       RenObj := Var'Access;                            -- OK.
       RenObj := Cnst'Access;                           -- ERROR:
       RenObj := AVar;                                  -- OK.
       RenObj := ACnst;                                 -- ERROR:
       RenObj := NCnst;                                 -- ERROR:
   end;

   -- Anonymous access formal objects:
   declare
       generic
           O : access Designated;
       procedure Test8;

       procedure Test8 is
       begin
           O.Item := 10;                                -- OK.
           O.Ptr := Int_Cnst'Access;                    -- OK.
       end Test8;

       procedure Do_It1 is new Test8 (Var'Access);      -- OK.
       procedure Do_It2 is new Test8 (Cnst'Access);     -- ERROR:
       procedure Do_It3 is new Test8 (AVar);            -- OK.
       procedure Do_It4 is new Test8 (ACnst);           -- ERROR:
       procedure Do_It5 is new Test8 (NCnst);           -- ERROR:
   begin
       Do_It1;
   end;
end B3A0006;

