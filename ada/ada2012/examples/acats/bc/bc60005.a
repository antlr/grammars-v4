-- BC60005.A
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
--     For a generic formal subprogram with a parameter or result type
--     that has a null_exclusion or is an access_definition with a
--     null_exclusion, check that an instantiation is illegal if the subtype
--     of the corresponding parameter or result of the actual subprogram
--     does not exclude null.
--
--     For an instance of a generic with a generic formal subprogram with
--     a parameter or result type that has null_exclusion or is an
--     access_definition with a null_exclusion whose actual is a generic formal
--     subprogram of an outer generic and that appears in the specification
--     of the outer generic, check that an instantiation of the outer generic
--     is illegal if the if the corresponding parameter or result of the
--     actual subprogram does not exclude null.
--
--     For an instance of a generic with a generic formal subprogram with
--     a parameter or result type that has null_exclusion or is an
--     access_definition with a null_exclusion whose actual is a generic formal
--     subprogram of an outer generic and that appears in the body
--     of the outer generic, check that the instance is illegal if the
--     corresponding parameter or result of the formal subprogram does not
--     include a null_exclusion.
--
-- CHANGE HISTORY:
--     12 Jun 2018 RLB Created test from similar BC40003.
--     26 Jul 2018 RLB Corrected the middle test objective.
--     17 Mar 2020 RLB Corrected results for InstG51, InstG61, InstGF51, and
--                     InstGF61.
--     12 May 2020 RLB Corrected test case for InstGF1 @437.
--
procedure BC60005 is

   type Int_Ptr is access all Integer;
   subtype NN_Int_Ptr is not null Int_Ptr;
   subtype NN2_Int_Ptr is NN_Int_Ptr;
   type Int_PS_Ptr is access Integer;
   subtype ON_Int_PS_Ptr is Int_PS_Ptr;
   subtype NN_Int_PS_Ptr is not null Int_PS_Ptr;

   procedure Proc1 (P1 : in out Int_Ptr) is
   begin
      null;
   end Proc1;

   procedure Proc2 (P2 : in out NN_Int_Ptr) is
   begin
      null;
   end Proc2;

   procedure Proc3 (P3 : in out NN2_Int_Ptr) is
   begin
      null;
   end Proc3;

   procedure Proc4 (P4 : access Integer) is
   begin
      null;
   end Proc4;

   procedure Proc5 (P5 : not null access Integer) is
   begin
      null;
   end Proc5;

   function Func4 (P : access Integer) return access Integer is
   begin
      return P;
   end Func4;

   function Func5 (P : access Integer) return not null access Integer is
   begin
      return P;
   end Func5;

   procedure Proc6 (P6 : in Int_PS_Ptr) is
   begin
      null;
   end Proc6;

   procedure Proc7 (P7 : in ON_Int_PS_Ptr) is
   begin
      null;
   end Proc7;

   procedure Proc8 (P8 : in NN_Int_PS_Ptr) is
   begin
      null;
   end Proc8;

   function Func6 (P : in Int_PS_Ptr) return Int_PS_Ptr is
   begin
      return P;
   end Func6;

   function Func7 (P : in Int_PS_Ptr) return ON_Int_PS_Ptr is
   begin
      return P;
   end Func7;

   function Func8 (P : in Int_PS_Ptr) return NN_Int_PS_Ptr is
   begin
      return P;
   end Func8;

   generic
      with procedure GProc0 (P : in out Int_Ptr);
   package Gen00 is
      B : Boolean := True;
   end Gen00;

   generic
      with procedure GProc1 (P : in out not null Int_Ptr);
   package Gen01 is
      B : Boolean := True;
   end Gen01;

   generic
      with procedure GProc2 (P : not null access Integer);
   package Gen02 is
      B : Boolean := False;
   end Gen02;

   generic
      with function GFunc2(P : access Integer) return not null access Integer;
   package GenF2 is
      B : Boolean := False;
   end GenF2;

   generic
      with procedure GProc3 (P : in not null Int_PS_Ptr);
   package Gen03 is
      B : Boolean := False;
   end Gen03;

   generic
      with function GFunc3 (P : in Int_PS_Ptr) return not null Int_PS_Ptr;
   package GenF3 is
      B : Boolean := False;
   end GenF3;

   type Func_Ptr is access function (X : Float) return Float;
   subtype NN_Func_Ptr is not null Func_Ptr;
   subtype NN2_Func_Ptr is NN_Func_Ptr;

   procedure ProcA (PA : in Func_Ptr) is
   begin
      null;
   end ProcA;

   procedure ProcB (PB : in NN_Func_Ptr) is
   begin
      null;
   end ProcB;

   procedure ProcC (PC : in NN2_Func_Ptr) is
   begin
      null;
   end ProcC;

   procedure ProcD (PD : access function (X : Float) return Float) is
   begin
      null;
   end ProcD;

   procedure ProcE (PE : not null access function (X : Float) return Float) is
   begin
      null;
   end ProcE;

   function FuncA (P : in Func_Ptr) return Func_Ptr is
   begin
      return P;
   end FuncA;

   function FuncB (P : in Func_Ptr) return NN_Func_Ptr is
   begin
      return P;
   end FuncB;

   function FuncC (P : in Func_Ptr) return NN2_Func_Ptr is
   begin
      return P;
   end FuncC;

   function FuncD (P : access procedure) return access procedure is
   begin
      return P;
   end FuncD;

   function FuncE (P : access procedure) return not null access procedure is
   begin
      return P;
   end FuncE;

   generic
      with procedure GProc4 (P : in Func_Ptr);
   package Gen04 is
      B : Boolean := True;
   end Gen04;

   generic
      with procedure GProc5 (P : in not null Func_Ptr);
   package Gen05 is
      B : Boolean := True;
   end Gen05;

   generic
      with procedure GProc6 (P :
                         not null access function (X : Float) return Float);
   package Gen06 is
      B : Boolean := False;
   end Gen06;

   generic
      with function GFunc5 (P : in Func_Ptr) return not null Func_Ptr;
   package GenF5 is
      B : Boolean := True;
   end GenF5;

   generic
      with function GFunc6 (P : access procedure)
                               return not null access procedure;
   package GenF6 is
      B : Boolean := False;
   end GenF6;

begin

   declare
      package Inst11 is new Gen01 (Proc1);               -- ERROR: {7;1}
      package Inst21 is new Gen01 (Proc2);               -- OK. {7;1}
      package Inst31 is new Gen01 (Proc3);               -- OK. {7;1}
      package Inst41 is new Gen02 (Proc4);               -- ERROR: {7;1}
      package Inst51 is new Gen02 (Proc5);               -- OK. {7;1}
      package Inst61 is new Gen03 (Proc6);               -- ERROR: {7;1}
      package Inst71 is new Gen03 (Proc7);               -- ERROR: {7;1}
      package Inst81 is new Gen03 (Proc8);               -- OK. {7;1}
      package InstF41 is new GenF2 (Func4);              -- ERROR: {7;1}
      package InstF51 is new GenF2 (Func5);              -- OK. {7;1}
      package InstF61 is new GenF3 (Func6);              -- ERROR: {7;1}
      package InstF71 is new GenF3 (Func7);              -- ERROR: {7;1}
      package InstF81 is new GenF3 (Func8);              -- OK. {7;1}

      package InstX1 is new Gen00 (Proc1);               -- OK. {7;1}
      package InstY1 is new Gen00 (Proc2);               -- OK. {7;1}
      package InstZ1 is new Gen00 (Proc3);               -- OK. {7;1}


      package Pack is
         type A_Tagged is tagged null record;

         procedure Prim (A : access A_Tagged);
         procedure Not_Prim (B : access Integer);

         generic
            with procedure GProc1 (P : not null access A_Tagged);
         package Gen07 is
            B : Boolean := True;
         end Gen07;

      end Pack;

      package body Pack is
         procedure Prim (A : access A_Tagged) is
         begin
            null;
         end Prim;

         procedure Not_Prim (B : access Integer) is
         begin
            null;
         end Not_Prim;

         package InstPA is new Gen07 (Prim);             -- OK. {10;1}
            -- (A is a controlling parameter, see 3.10(13.1/2)).
         package InstPB is new Gen02 (Not_Prim);         -- ERROR: {10;1}
      end Pack;

      package InstA1 is new Gen05 (ProcA);               -- ERROR: {7;1}
      package InstB1 is new Gen05 (ProcB);               -- OK. {7;1}
      package InstC1 is new Gen05 (ProcC);               -- OK. {7;1}
      package InstD1 is new Gen06 (ProcD);               -- ERROR: {7;1}
      package InstE1 is new Gen06 (ProcE);               -- OK. {7;1}
      package InstF1 is new Gen04 (ProcA);               -- OK. {7;1}
      package InstG1 is new Gen04 (ProcB);               -- OK. {7;1}
      package InstH1 is new Gen04 (ProcC);               -- OK. {7;1}
      package InstFA1 is new GenF5 (FuncA);              -- ERROR: {7;1}
      package InstFB1 is new GenF5 (FuncB);              -- OK. {7;1}
      package InstFC1 is new GenF5 (FuncC);              -- OK. {7;1}
      package InstFD1 is new GenF6 (FuncD);              -- ERROR: {7;1}
      package InstFE1 is new GenF6 (FuncE);              -- OK. {7;1}

      -- Second objective:

      generic
         with procedure GProc1 (P : in out NN_Int_Ptr);
      package GenT1 is
         package Inst1 is new Gen01 (GProc1);            -- OK. {10;1}
      private
         package Inst1OK is new Gen00 (GProc1);          -- OK. {10;1}
      end GenT1;

      package Inst2_1 is new GenT1 (Proc2);              -- OK. {7;1}
      package Inst2_2 is new GenT1 (Proc1);              -- ERROR: {7;1}(Inst1)

      generic
         with procedure GProc2 (P : in out NN2_Int_Ptr);
      package GenT2 is
         package Inst2OK is new Gen00 (GProc2);          -- OK. {10;1}
      private
         package Inst2 is new Gen01 (GProc2);            -- OK. {10;1}
      end GenT2;

      package Inst2_3 is new GenT2 (Proc2);              -- OK. {7;1}
      package Inst2_4 is new GenT2 (Proc1);              -- ERROR: {7;1}(Inst2)

      generic
         with procedure GProc3 (P : in NN_Func_Ptr);
      package GenT3 is
         package Inst3 is new Gen05 (GProc3);            -- OK. {10;1}
      private
         package Inst3OK is new Gen04 (GProc3);          -- OK. {10;1}
      end GenT3;

      package Inst2_5 is new GenT3 (ProcB);              -- OK. {7;1}
      package Inst2_6 is new GenT3 (ProcA);              -- ERROR: {7;1}(Inst3)

      generic
         with procedure GProc4 (P : in NN2_Func_Ptr);
      package GenT4 is
         package Inst4OK is new Gen04 (GProc4);          -- OK. {10;1}
      private
         package Inst4 is new Gen05 (GProc4);            -- OK. {10;1}
      end GenT4;

      package Inst2_7 is new GenT4 (ProcB);              -- OK. {7;1}
      package Inst2_8 is new GenT4 (ProcA);              -- ERROR: {7;1}(Inst4)

      generic
         with procedure GProc5 (P : NN_Int_PS_Ptr);
      package GenT5 is
         package Inst5 is new Gen03 (GProc5);            -- OK. {10;1}
      end GenT5;

      package Inst2_9 is new GenT5 (Proc8);              -- OK. {7;1}
      package Inst2_A is new GenT5 (Proc6);              -- ERROR: {7;1}(Inst5)

      generic
         with function GFunc6 (P : in Int_PS_Ptr) return NN_Int_PS_Ptr;
      package GenT6 is
         package Inst6 is new GenF3 (GFunc6);            -- OK. {10;1}
      end GenT6;

      package Inst2_B is new GenT6 (Func8);              -- OK. {7;1}
      package Inst2_C is new GenT6 (Func6);              -- ERROR: {7;1}(Inst6)

      generic
         with function GFunc7 (P : Func_Ptr) return NN2_Func_Ptr;
      package GenT7 is
         B : Character := 'B';
      private
         package Inst7 is new GenF5 (GFunc7);            -- OK. {10;1}
      end GenT7;

      package Inst2_D is new GenT7 (FuncB);              -- OK. {7;1}
      package Inst2_E is new GenT7 (FuncA);              -- ERROR: {7;1}(Inst7)


      -- Third objective:

      generic
         with procedure GProc1 (P : in out NN_Int_Ptr);
         with procedure GProc2 (P : in out not null Int_Ptr);
         with procedure GProc3 (P : not null access Integer);
         with procedure GProc4 (P : access Integer);
         with procedure GProc5 (P : not null Int_PS_Ptr);
         with procedure GProc6 (P : NN_Int_PS_Ptr);
         with function GFunc3 (P : access Integer)
                                     return not null access Integer;
         with function GFunc4 (P : access Integer) return access Integer;
         with function GFunc5 (P : Int_PS_Ptr) return not null Int_PS_Ptr;
         with function GFunc6 (P : Int_PS_Ptr) return NN_Int_PS_Ptr;
      package GenTA is
         procedure Dummy;
      end GenTA;

      package body GenTA is

         package InstG11 is new Gen01 (GProc1);          -- ERROR: {10;1}
            -- Note: This case would be legal outside of the generic body.
         package InstG12 is new Gen00 (GProc1);          -- OK. {10;1}
         package InstG21 is new Gen01 (GProc2);          -- OK. {10;1}
         package InstG31 is new Gen02 (GProc3);          -- OK. {10;1}
         package InstG41 is new Gen02 (GProc4);          -- ERROR: {10;1}
         package InstG51 is new Gen03 (GProc5);          -- OK. {10;1}
         package InstG61 is new Gen03 (GProc6);          -- ERROR: {10;1}
         package InstGF31 is new GenF2 (GFunc3);         -- OK. {10;1}
         package InstGF41 is new GenF2 (GFunc4);         -- ERROR: {10;1}
         package InstGF51 is new GenF3 (GFunc5);         -- OK. {10;1}
         package InstGF61 is new GenF3 (GFunc6);         -- ERROR: {10;1}

         procedure Dummy is
            package InstG71 is new Gen01 (GProc1);       -- ERROR: {13;1}
            package InstG81 is new Gen02 (GProc4);       -- ERROR: {13;1}
            package InstGF1 is new GenF3 (GFunc6);       -- ERROR: {13;1}
         begin
            null;
         end Dummy;

      end GenTA;

      generic
         with procedure GProc1 (P : NN_Func_Ptr);
         with procedure GProc2 (P : not null Func_Ptr);
         with procedure GProc3 (P :
                           not null access function (X : Float) return Float);
         with procedure GProc4 (P : access function (X : Float) return Float);
         with function GFunc1 (P : Func_Ptr) return NN_Func_Ptr;
         with function GFunc2 (P : Func_Ptr) return not null Func_Ptr;
         with function GFunc3 (P : access procedure)
                                             return not null access procedure;
         with function GFunc4 (P : access procedure) return access procedure;
      package GenTB is
         package Nest is
            procedure Dummy;
         end Nest;
      end GenTB;

      package body GenTB is

         package InstG11 is new Gen05 (GProc1);          -- ERROR: {10;1}
            -- Note: This case would be legal outside of the generic body.
         package InstG12 is new Gen04 (GProc1);          -- OK. {10;1}
         package InstG21 is new Gen05 (GProc2);          -- OK. {10;1}
         package InstG31 is new Gen06 (GProc3);          -- OK. {10;1}
         package InstG41 is new Gen06 (GProc4);          -- ERROR: {10;1}
         package InstGF1 is new GenF5 (GFunc1);          -- ERROR: {10;1}
            -- Note: This case would be legal outside of the generic body.
         package InstGF2 is new GenF5 (GFunc2);          -- OK. {10;1}
         package InstGF3 is new GenF6 (GFunc3);          -- OK. {10;1}
         package InstGF4 is new GenF6 (GFunc4);          -- ERROR: {10;1}

         package body Nest is
            package InstG51 is new Gen05 (GProc1);       -- ERROR: {13;1}
            package InstG52 is new Gen04 (GProc1);       -- OK. {13;1}
            package InstG61 is new Gen05 (GProc2);       -- OK. {13;1}
            package InstG71 is new Gen06 (GProc3);       -- OK. {13;1}
            package InstG81 is new Gen06 (GProc4);       -- ERROR: {13;1}
            package InstGF1 is new GenF5 (GFunc1);       -- ERROR: {13;1}
            package InstGF2 is new GenF5 (GFunc2);       -- OK. {13;1}
            package InstGF3 is new GenF6 (GFunc3);       -- OK. {13;1}
            package InstGF4 is new GenF6 (GFunc4);       -- ERROR: {13;1}

            procedure Dummy is
               package InstG91 is new Gen05 (GProc1);    -- ERROR: {16;1}
               package InstGF9 is new GenF5 (GFunc1);    -- ERROR: {16;1}
            begin
               null;
            end Dummy;
         end Nest;

      end GenTB;

   begin
      null;
   end;
end BC60005;
