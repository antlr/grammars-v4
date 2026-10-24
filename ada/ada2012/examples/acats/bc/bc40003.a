-- BC40003.A
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
-- OBJECTIVE:
--     For a generic formal in out object with a null_exclusion or an
--     access_definition with a null_exclusion, check that an instantiation
--     is illegal if the subtype of the actual object does not exclude null.
--
--     For an instance of a generic in the specification of an outer
--     generic with a generic formal in out object with a
--     null_exclusion or an access_definition with a null_exclusion whose
--     actual is a null excluding generic formal in out object of the outer
--     generic, check that an instantiation of the outer generic is illegal if
--     the subtype of the actual object does not exclude null.
--
--     For an instance of a generic in the body of an outer
--     generic with a generic formal in out object with a
--     null_exclusion or an access_definition with a null_exclusion whose
--     actual is a null excluding generic formal in out object of the
--     outer generic, check that the instance is illegal if the formal object
--     does not include a null_exclusion.
--
-- CHANGE HISTORY:
--     12 Jun 2018 RLB Created test from similar B851004.
--     06 Sep 2018 RLB Corrected test to remove cases whose results might
--                     change because of AI12-0287-1. (They should be replaced
--                     in the Ada 2020 test suite.) Added additional "in out"
--                     cases to cover cases that won't change by that AI.
--
procedure BC40003 is

   type Int_Ptr is access all Integer;
   subtype NN_Int_Ptr is not null Int_Ptr;
   subtype NN2_Int_Ptr is NN_Int_Ptr;
   type Int_PS_Ptr is access Integer;
   subtype ON_Int_PS_Ptr is Int_PS_Ptr;
   subtype NN_Int_PS_Ptr is not null Int_PS_Ptr;

   Obj1 : Int_Ptr;
   Obj2 : NN_Int_Ptr;
   Obj3 : NN2_Int_Ptr;
   Obj4 : access Integer;
   Obj5 : not null access Integer;
   Obj6 : Int_PS_Ptr;
   Obj7 : ON_Int_PS_Ptr;
   Obj8 : NN_Int_PS_Ptr;

   generic
      GObj0 : in out Int_Ptr;
   package Gen00 is
      B : Boolean := GObj0 /= null;
   end Gen00;

   generic
      GObj1 : in out not null Int_Ptr;
   package Gen01 is
      B : Boolean := GObj1 /= null;
   end Gen01;

   generic
      GObj2 : not null access Integer;
   package Gen02 is
      B : Boolean := False;
   end Gen02;

   generic
      GObj2 : in out not null access Integer;
   package Gen02A is
      B : Boolean := False;
   end Gen02A;

   generic
      GObj3 : not null Int_PS_Ptr;
   package Gen03 is
      B : Boolean := GObj3 /= null;
   end Gen03;

   generic
      GObj3 : in out not null Int_PS_Ptr;
   package Gen03A is
      B : Boolean := GObj3 /= null;
   end Gen03A;

   type Func_Ptr is access function (X : Float) return Float;
   subtype NN_Func_Ptr is not null Func_Ptr;
   subtype NN2_Func_Ptr is NN_Func_Ptr;

   ObjA : Func_Ptr;
   ObjB : NN_Func_Ptr;
   ObjC : NN2_Func_Ptr;
   ObjD : access function (X : Float) return Float;
   ObjE : not null access function (X : Float) return Float;

   generic
      GObj4 : in out Func_Ptr;
   package Gen04 is
      B : Boolean := GObj4 /= null;
   end Gen04;

   generic
      GObj5 : in out not null Func_Ptr;
   package Gen05 is
      B : Boolean := GObj5 /= null;
   end Gen05;

   generic
      GObj6 : in out not null access function (X : Float) return Float;
   package Gen06 is
      B : Boolean := False;
   end Gen06;

begin

   declare
      package Inst11 is new Gen01 (Obj1);                -- ERROR: {7;1}
      package Inst21 is new Gen01 (Obj2);                -- OK. {7;1}
      package Inst31 is new Gen01 (Obj3);                -- OK. {7;1}
      --package Inst41 is new Gen02 (Obj4);              -- OK. {7;1}
         -- Above is OK by AI12-0287-1, but was illegal by the original rules.
      package Inst51 is new Gen02 (Obj5);                -- OK. {7;1}
      package Inst61 is new Gen02A (Obj4);               -- ERROR: {7;1}
      package Inst71 is new Gen02A (Obj5);               -- OK. {7;1}
      --package Inst81 is new Gen03 (Obj6);              -- OK. {7;1}
         -- Above is OK by AI12-0287-1, but was illegal by the original rules.
      --package Inst91 is new Gen03 (Obj7);              -- OK. {7;1}
         -- Above is OK by AI12-0287-1, but was illegal by the original rules.
      package InstA1 is new Gen03 (Obj8);                -- OK. {7;1}
      package InstB1 is new Gen03A (Obj6);               -- ERROR: {7;1}
      package InstC1 is new Gen03A (Obj7);               -- ERROR: {7;1}
      package InstD1 is new Gen03A (Obj8);               -- OK. {7;1}

      package InstX1 is new Gen00 (Obj1);                -- OK. {7;1}
      package InstY1 is new Gen00 (Obj2);                -- OK. {7;1}
      package InstZ1 is new Gen00 (Obj3);                -- OK. {7;1}


      package Pack is
         type A_Tagged is tagged null record;

         procedure Prim (A : access A_Tagged; B : access Integer);

         generic
            GObj7 : not null access A_Tagged;
         package Gen07 is
            B : Boolean := True;
         end Gen07;

      end Pack;

      package body Pack is
         procedure Prim (A : access A_Tagged; B : access Integer) is
            package InstPA is new Gen07 (A);             -- OK. {13;1}
               -- (A is a controlling parameter, see 3.10(13.1/2)).
            --package InstPB is new Gen02 (B);           -- OK. {13;1}
                 -- Above is OK by AI12-0287-1, but was an error by the
                 -- original rules.
            -- Note: We cannot write an error case here, as B is a constant,
            -- an a generic in out parameter requires a variable.
         begin
            null;
         end Prim;
      end Pack;

      package InstG1 is new Gen05 (ObjA);                -- ERROR: {7;1}
      package InstH1 is new Gen05 (ObjB);                -- OK. {7;1}
      package InstI1 is new Gen05 (ObjC);                -- OK. {7;1}
      package InstJ1 is new Gen06 (ObjD);                -- ERROR: {7;1}
      package InstK1 is new Gen06 (ObjE);                -- OK. {7;1}
      package InstL1 is new Gen04 (ObjA);                -- OK. {7;1}
      package InstM1 is new Gen04 (ObjB);                -- OK. {7;1}
      package InstN1 is new Gen04 (ObjC);                -- OK. {7;1}

      -- Second objective:

      generic
         GObj1 : in out NN_Int_Ptr;
      package GenT1 is
         package Inst1 is new Gen01 (GObj1);             -- OK. {10;1}
      private
         package Inst1OK is new Gen00 (GObj1);           -- OK. {10;1}
      end GenT1;

      package Inst2_1 is new GenT1 (Obj2);               -- OK. {7;1}
      package Inst2_2 is new GenT1 (Obj1);               -- ERROR: {7;1}(Inst1)

      generic
         GObj2 : in out NN2_Int_Ptr;
      package GenT2 is
         package Inst2OK is new Gen00 (GObj2);           -- OK. {10;1}
      private
         package Inst2 is new Gen01 (GObj2);             -- OK. {10;1}
      end GenT2;

      package Inst2_3 is new GenT2 (Obj2);               -- OK. {7;1}
      package Inst2_4 is new GenT2 (Obj1);               -- ERROR: {7;1}(Inst2)

      generic
         GObj3 : in out NN_Func_Ptr;
      package GenT3 is
         package Inst3 is new Gen05 (GObj3);             -- OK. {10;1}
      private
         package Inst3OK is new Gen04 (GObj3);           -- OK. {10;1}
      end GenT3;

      package Inst2_5 is new GenT3 (ObjB);               -- OK. {7;1}
      package Inst2_6 is new GenT3 (ObjA);               -- ERROR: {7;1}(Inst3)

      generic
         GObj4 : in out NN2_Func_Ptr;
      package GenT4 is
         package Inst4OK is new Gen04 (GObj4);           -- OK. {10;1}
      private
         package Inst4 is new Gen05 (GObj4);             -- OK. {10;1}
      end GenT4;

      package Inst2_7 is new GenT4 (ObjB);               -- OK. {7;1}
      package Inst2_8 is new GenT4 (ObjA);               -- ERROR: {7;1}(Inst4)

      generic
         GObj5 : NN_Int_PS_Ptr;
      package GenT5 is
         package Inst5 is new Gen03 (GObj5);             -- OK. {10;1}
      end GenT5;

      package Inst2_9 is new GenT5 (Obj8);               -- OK. {7;1}
      --package Inst2_A is new GenT5 (Obj6);             -- OK. {7;1}(Inst5)
         -- Above is OK by AI12-0287-1, but was illegal by the original rules.

      generic
         GObj6 : in out NN_Int_PS_Ptr;
      package GenT6 is
         package Inst6 is new Gen03A (GObj6);            -- OK. {10;1}
      end GenT6;

      package Inst2_B is new GenT6 (Obj8);               -- OK. {7;1}
      package Inst2_C is new GenT6 (Obj6);               -- ERROR: {7;1}(Inst6)


      -- Third objective:

      generic
         GObj1 : in out NN_Int_Ptr;
         GObj2 : in out not null Int_Ptr;
         GObj3 : not null access Integer;
         GObj4 : access Integer;
         GObj5 : not null Int_PS_Ptr;
         GObj6 : NN_Int_PS_Ptr;
         GObj7 : in out not null Int_PS_Ptr;
         GObj8 : in out NN_Int_PS_Ptr;
      package GenTA is
         procedure Dummy;
      end GenTA;

      package body GenTA is

         package InstG11 is new Gen01 (GObj1);           -- ERROR: {10;1}
            -- Note: This case would be legal outside of the generic body.
         package InstG12 is new Gen00 (GObj1);           -- OK. {10;1}
         package InstG21 is new Gen01 (GObj2);           -- OK. {10;1}
         package InstG31 is new Gen02 (GObj3);           -- OK. {10;1}
         --package InstG41 is new Gen02 (GObj4);         -- OK. {10;1}
         -- Above is OK by AI12-0287-1, but was illegal by the original rules.
         package InstG51 is new Gen03 (GObj5);           -- OK. {10;1}
         --package InstG61 is new Gen03 (GObj6);         -- OK. {10;1}
         -- Above is OK by AI12-0287-1, but was illegal by the original rules.
         package InstG71 is new Gen03A (GObj7);          -- OK. {10;1}
         package InstG81 is new Gen03A (GObj8);          -- ERROR: {10;1}

         procedure Dummy is
            package InstG91 is new Gen01 (GObj1);        -- ERROR: {13;1}
            --package InstGA1 is new Gen02 (GObj4);      -- OK. {13;1}
              -- Above is OK by AI12-0287-1, but was an error by the
              -- original rules.
            package InstGB1 is new Gen03A (GObj8);       -- ERROR: {13;1}
         begin
            null;
         end Dummy;

      end GenTA;

      generic
         GObj1 : in out NN_Func_Ptr;
         GObj2 : in out not null Func_Ptr;
         GObj3 : in out not null access function (X : Float) return Float;
         GObj4 : in out access function (X : Float) return Float;
      package GenTB is
         package Nest is
            procedure Dummy;
         end Nest;
      end GenTB;

      package body GenTB is

         package InstG11 is new Gen05 (GObj1);           -- ERROR: {10;1}
            -- Note: This case would be legal outside of the generic body.
         package InstG12 is new Gen04 (GObj1);           -- OK. {10;1}
         package InstG21 is new Gen05 (GObj2);           -- OK. {10;1}
         package InstG31 is new Gen06 (GObj3);           -- OK. {10;1}
         package InstG41 is new Gen06 (GObj4);           -- ERROR: {10;1}

         package body Nest is
            package InstG51 is new Gen05 (GObj1);        -- ERROR: {13;1}
            package InstG52 is new Gen04 (GObj1);        -- OK. {13;1}
            package InstG61 is new Gen05 (GObj2);        -- OK. {13;1}
            package InstG71 is new Gen06 (GObj3);        -- OK. {13;1}
            package InstG81 is new Gen06 (GObj4);        -- ERROR: {13;1}

            procedure Dummy is
               package InstG91 is new Gen05 (GObj1);     -- ERROR: {16;1}
            begin
               null;
            end Dummy;
         end Nest;

      end GenTB;

   begin
      null;
   end;
end BC40003;
