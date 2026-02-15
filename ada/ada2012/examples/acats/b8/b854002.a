-- B854002.A
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
--     For a subprogram renaming with a parameter or result type
--     that has a null_exclusion or is an access_definition with a
--     null_exclusion, check that the renaming is illegal if the subtype
--     of the corresponding parameter or result of the renamed subprogram
--     does not exclude null.
--
--     For a subprogram renaming with a parameter or result type
--     that has a null_exclusion or is an access_definition with a
--     null_exclusion that appears in the specification of an outer
--     generic whose renamed subprogram is a generic formal subprogram of
--     the outer generic, check that an instantiation of the outer generic is
--     illegal if the corresponding parameter or result of the actual
--     subprogram does not exclude null.
--
--     For a subprogram renaming with a parameter or result type
--     that has a null_exclusion or is an access_definition with a
--     null_exclusion that appears in the body of an outer
--     generic whose renamed subprogram is a generic formal subprogram of
--     the outer generic, check that the renaming is illegal if the
--     corresponding parameter or result of the formal subprogram does not
--     include a null_exclusion.
--
-- CHANGE HISTORY:
--     13 Jun 2018 RLB Created test from similar BC60005.
--     26 Jul 2018 RLB Corrected the wording of the middle test objective.
--     13 May 2020 RLB Corrected results for RenG51, RenG61, RenGF51, and
--                     RenGF61. Corrected test case for InstGF1 @412.
--
procedure B854002 is

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

begin

   declare
      procedure Ren11 (P : in out not null Int_Ptr)
                                      renames Proc1;     -- ERROR: {1:7;1}
      procedure Ren21 (P : in out not null Int_Ptr)
                                      renames Proc2;     -- OK. {1:7;1}
      procedure Ren31 (P : in out not null Int_Ptr)
                                      renames Proc3;     -- OK. {1:7;1}
      procedure Ren41 (P : not null access Integer)
                             renames Proc4;              -- ERROR: {1:7;1}
      procedure Ren51 (P : not null access Integer)
                             renames Proc5;              -- OK. {1:7;1}
      procedure Ren61 (P : in not null Int_PS_Ptr)
                             renames Proc6;              -- ERROR: {1:7;1}
      procedure Ren71 (P : in not null Int_PS_Ptr)
                             renames Proc7;              -- ERROR: {1:7;1}
      procedure Ren81 (P : in not null Int_PS_Ptr)
                             renames Proc8;              -- OK. {1:7;1}
      function RenF41 (P : access Integer)
         return not null access Integer renames Func4;   -- ERROR: {1:7;1}
      function RenF51 (P : access Integer)
         return not null access Integer renames Func5;   -- OK. {1:7;1}
      function RenF61 (P : in Int_PS_Ptr)
         return not null Int_PS_Ptr renames Func6;       -- ERROR: {1:7;1}
      function RenF71 (P : in Int_PS_Ptr)
         return not null Int_PS_Ptr renames Func7;       -- ERROR: {1:7;1}
      function RenF81 (P : in Int_PS_Ptr)
         return not null Int_PS_Ptr renames Func8;       -- OK. {1:7;1}

      procedure RenX1 (P : in out Int_Ptr) renames Proc1;-- OK. {7;1}
      procedure RenY1 (P : in out Int_Ptr) renames Proc2;-- OK. {7;1}
      procedure RenZ1 (P : in out Int_Ptr) renames Proc3;-- OK. {7;1}

      package Pack is
         type A_Tagged is tagged null record;

         procedure Prim (A : access A_Tagged);
         procedure Not_Prim (B : access Integer);

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

         procedure RenPA (P : not null access A_Tagged)
                             renames Prim;               -- OK. {1:10;1}
            -- (A is a controlling parameter, see 3.10(13.1/2)).
         procedure RenPB (P : not null access Integer)
                             renames Not_Prim;           -- ERROR: {1:10;1}
      end Pack;

      procedure RenA1 (P : in not null Func_Ptr)
                                      renames ProcA;     -- ERROR: {1:7;1}
      procedure RenB1 (P : in not null Func_Ptr)
                                      renames ProcB;     -- OK. {1:7;1}
      procedure RenC1 (P : in not null Func_Ptr)
                                      renames ProcC;     -- OK. {1:7;1}
      procedure RenD1 (P :
                 not null access function (X : Float) return Float)
                                   renames ProcD;        -- ERROR: {2:7;1}
      procedure RenE1 (P :
                 not null access function (X : Float) return Float)
                                   renames ProcE;        -- OK. {2:7;1}
      procedure RenF1 (P : in Func_Ptr) renames ProcA;   -- OK. {7;1}
      procedure RenG1 (P : in Func_Ptr) renames ProcB;   -- OK. {7;1}
      procedure RenH1 (P : in Func_Ptr) renames ProcC;   -- OK. {7;1}
      function RenFA1 (P : in Func_Ptr)
                 return not null Func_Ptr renames FuncA; -- ERROR: {1:7;1}
      function RenFB1 (P : in Func_Ptr)
                 return not null Func_Ptr renames FuncB; -- OK. {1:7;1}
      function RenFC1 (P : in Func_Ptr)
                 return not null Func_Ptr renames FuncC; -- OK. {1:7;1}
      function RenFD1 (P : access procedure)
                           return not null access procedure
                                   renames FuncD;        -- ERROR: {2:7;1}
      function RenFE1 (P : access procedure)
                           return not null access procedure
                                   renames FuncE;        -- OK. {2:7;1}

      -- Second objective:

      generic
         with procedure GProc1 (P : in out NN_Int_Ptr);
      package GenT1 is
         procedure Ren1 (P : in out not null Int_Ptr)
                                      renames GProc1;    -- OK. {1:10;1}
      private
         procedure Ren1OK (P : in out Int_Ptr)
                                       renames GProc1;   -- OK. {1:10;1}
      end GenT1;

      package Inst2_1 is new GenT1 (Proc2);              -- OK. {7;1}
      package Inst2_2 is new GenT1 (Proc1);              -- ERROR: {7;1} (Ren1)

      generic
         with procedure GProc2 (P : in out NN2_Int_Ptr);
      package GenT2 is
         procedure Ren2OK (P : in out Int_Ptr)
                                       renames GProc2;   -- OK. {1:10;1}
      private
         procedure Ren2 (P : in out not null Int_Ptr)
                                      renames GProc2;    -- OK. {1:10;1}
      end GenT2;

      package Inst2_3 is new GenT2 (Proc2);              -- OK. {7;1}
      package Inst2_4 is new GenT2 (Proc1);              -- ERROR: {7;1} (Ren2)

      generic
         with procedure GProc3 (P : in NN_Func_Ptr);
      package GenT3 is
         procedure Ren3 (P : in not null Func_Ptr)
                                      renames GProc3;    -- OK. {1:10;1}
      private
         procedure Ren3OK (P : in Func_Ptr) renames GProc3; -- OK. {10;1}
      end GenT3;

      package Inst2_5 is new GenT3 (ProcB);              -- OK. {7;1}
      package Inst2_6 is new GenT3 (ProcA);              -- ERROR: {7;1} (Ren3)

      generic
         with procedure GProc4 (P : in NN2_Func_Ptr);
      package GenT4 is
         procedure Ren4OK (P : in Func_Ptr) renames GProc4; -- OK. {10;1}
      private
         procedure Ren4 (P : in not null Func_Ptr)
                                      renames GProc4;    -- OK. {1:10;1}
      end GenT4;

      package Inst2_7 is new GenT4 (ProcB);              -- OK. {7;1}
      package Inst2_8 is new GenT4 (ProcA);              -- ERROR: {7;1} (Ren4)

      generic
         with procedure GProc5 (P : NN_Int_PS_Ptr);
      package GenT5 is
         procedure Ren5 (P : in not null Int_PS_Ptr)
                             renames GProc5;             -- OK. {1:10;1}
      end GenT5;

      package Inst2_9 is new GenT5 (Proc8);              -- OK. {7;1}
      package Inst2_A is new GenT5 (Proc6);              -- ERROR: {7;1} (Ren5)

      generic
         with function GFunc6 (P : in Int_PS_Ptr) return NN_Int_PS_Ptr;
      package GenT6 is
         function Ren6 (P : in Int_PS_Ptr)
            return not null Int_PS_Ptr renames GFunc6;   -- OK. {1:10;1}
      end GenT6;

      package Inst2_B is new GenT6 (Func8);              -- OK. {7;1}
      package Inst2_C is new GenT6 (Func6);              -- ERROR: {7;1} (Ren6)

      generic
         with function GFunc7 (P : Func_Ptr) return NN2_Func_Ptr;
      package GenT7 is
         B : Character := 'B';
      private
         function Ren7 (P : in Func_Ptr)
                return not null Func_Ptr renames GFunc7; -- OK. {1:10;1}
      end GenT7;

      package Inst2_D is new GenT7 (FuncB);              -- OK. {7;1}
      package Inst2_E is new GenT7 (FuncA);              -- ERROR: {7;1} (Ren7)


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

         procedure RenG11 (P : in out not null Int_Ptr)
                                      renames GProc1;    -- ERROR: {1:10;1}
            -- Note: This case would be legal outside of the generic body.
         procedure RenG12 (P : in out Int_Ptr)
                                       renames GProc1;   -- OK. {1:10;1}
         procedure RenG21 (P : in out not null Int_Ptr)
                                      renames GProc2;    -- OK. {1:10;1}
         procedure RenG31 (P : not null access Integer)
                             renames GProc3;             -- OK. {1:10;1}
         procedure RenG41 (P : not null access Integer)
                             renames GProc4;             -- ERROR: {1:10;1}
         procedure RenG51 (P : in not null Int_PS_Ptr)
                             renames GProc5;             -- OK. {1:10;1}
         procedure RenG61 (P : in not null Int_PS_Ptr)
                             renames GProc6;             -- ERROR: {1:10;1}
         function RenGF31 (P : access Integer)
            return not null access Integer renames GFunc3;-- OK. {1:10;1}
         function RenGF41 (P : access Integer)
            return not null access Integer renames GFunc4;-- ERROR: {1:10;1}
         function RenGF51 (P : in Int_PS_Ptr)
            return not null Int_PS_Ptr renames GFunc5;   -- OK. {1:10;1}
         function RenGF61 (P : in Int_PS_Ptr)
            return not null Int_PS_Ptr renames GFunc6;   -- ERROR: {1:10;1}

         procedure Dummy is
            procedure RenG71 (P : in out not null Int_Ptr)
                                      renames GProc1;    -- ERROR: {1:13;1}
            procedure RenG81 (P : not null access Integer)
                             renames GProc4;             -- ERROR: {1:13;1}
            function RenGF1 (P : in Int_PS_Ptr)
               return not null Int_PS_Ptr renames GFunc6;-- ERROR: {1:13;1}
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

         procedure RenG11 (P : in not null Func_Ptr)
                                      renames GProc1;    -- ERROR: {1:10;1}
            -- Note: This case would be legal outside of the generic body.
         procedure RenG12 (P : in Func_Ptr) renames GProc1; -- OK. {10;1}
         procedure RenG21 (P : in not null Func_Ptr)
                                      renames GProc2;    -- OK. {1:10;1}
         procedure RenG31 (P :
                 not null access function (X : Float) return Float)
                                   renames GProc3;       -- OK. {2:10;1}
         procedure RenG41 (P :
                 not null access function (X : Float) return Float)
                                   renames GProc4;       -- ERROR: {2:10;1}
         function RenGF1 (P : in Func_Ptr)
                return not null Func_Ptr renames GFunc1; -- ERROR: {1:10;1}
            -- Note: This case would be legal outside of the generic body.
         function RenGF2 (P : in Func_Ptr)
                return not null Func_Ptr renames GFunc2; -- OK. {1:10;1}
         function RenGF3 (P : access procedure)
                           return not null access procedure
                                   renames GFunc3;       -- OK. {2:10;1}
         function RenGF4 (P : access procedure)
                           return not null access procedure
                                   renames GFunc4;       -- ERROR: {2:10;1}

         package body Nest is
            procedure RenG51 (P : in not null Func_Ptr)
                                      renames GProc1;    -- ERROR: {1:13;1}
            procedure RenG52 (P : in Func_Ptr) renames GProc1; -- OK. {13;1}
            procedure RenG61 (P : in not null Func_Ptr)
                                      renames GProc2;    -- OK. {1:13;1}
            procedure RenG71 (P :
                 not null access function (X : Float) return Float)
                                   renames GProc3;       -- OK. {2:13;1}
            procedure RenG81 (P :
                 not null access function (X : Float) return Float)
                                   renames GProc4;       -- ERROR: {2:13;1}
            function RenGF1 (P : in Func_Ptr)
                return not null Func_Ptr renames GFunc1; -- ERROR: {1:13;1}
            function RenGF2 (P : in Func_Ptr)
                return not null Func_Ptr renames GFunc2; -- OK. {1:13;1}
            function RenGF3 (P : access procedure)
                           return not null access procedure
                                   renames GFunc3;       -- OK. {2:13;1}
            function RenGF4 (P : access procedure)
                           return not null access procedure
                                   renames GFunc4;       -- ERROR: {2:13;1}

            procedure Dummy is
               procedure RenG51 (P : in not null Func_Ptr)
                                      renames GProc1;    -- ERROR: {1:16;1}
               function RenGF1 (P : in Func_Ptr)
                return not null Func_Ptr renames GFunc1; -- ERROR: {1:16;1}
            begin
               null;
            end Dummy;
         end Nest;

      end GenTB;

   begin
      null;
   end;
end B854002;
