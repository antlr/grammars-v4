-- B954005.A
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
--      Check that the target protected object of a requeue is a variable.
--
-- TEST DESCRIPTION:
--      The Binding Interpretations AI05-0225-1 and AI05-0291-1 clarify that
--      this objective is intended (although it has been intended in every
--      version of Ada since Ada 9x).
--
--      We try to test at least one case of almost every kind of constant that
--      can be a protected object:
--          * a object_declaration with constant;
--          * a formal parameter of mode in;
--          * a generic formal object of mode in;
--          * the dereference of an access-to-constant value;
--          * the result of evaluating a function_call;
--          * a selected_component or indexed_component of a constant.
--
--      Note that we don't try a constant return object in an extended
--      return statement, as it's unclear how we could do that and still
--      be in a context where a requeue is allowed. We also don't try slices
--      or view conversions of constants as those seem like overkill (they
--      require further reduction to work).
--
--      Similarly, we don't try within the body of a protected function,
--      because a requeue is illegal there no matter how the prefix is
--      interpreted.
--
--      We only try parameterless requeues; true completeness would require
--      trying some requeues with parameters, but that's hard to do when
--      also passing parameters to use as targets.
--
-- CHANGE HISTORY:
--       5 JUL 12   RLB     Created test from B954003 (which unintentionally
--                          had some of these illegal cases).
--      28 FEB 14   RLB     Added previously commented out Ada 2012 cases
--                          for ACATS 4.0.
--
--!


procedure B954005 is

   protected type POT1 is
      entry PE11;
   end POT1;

   protected body POT1 is
      entry PE11 when True is
      begin
         null;
      end PE11;
   end POT1;

   function Prot_Func return POT1 is
   begin
      return Obj:POT1;
   end Prot_Func;


   Prot_Variable : POT1;
   Prot_Constant : constant POT1 := Prot_Func;

   type Prot_Acc  is access POT1;
   type Prot_CAcc is access constant POT1;
   Prot_A_Var  : Prot_Acc  := new POT1;
   Prot_A_Cnst : Prot_CAcc := new POT1'(Prot_Func);

   type P_Rec is record
      POT : POT1;
   end record;

   Prot_R_Var  : P_Rec;
   Prot_R_Cnst : constant P_Rec := (POT => Prot_Func);

   type P_Arr is array (1..3) of POT1;
   Prot_Arr_Var  : P_Arr;
   Prot_Arr_Cnst : constant P_Arr := (1..3 => Prot_Func);

   package Nested is
      type Intf is synchronized interface;
      procedure PE12 (Param : in out Intf) is abstract
         with Synchronization => By_Entry;
   end Nested;

   ---------------

   protected type POT2 is
      entry PE21 (Prot_In_Parm     : in     POT1;
                  Prot_In_Out_Parm : in out POT1;
		  Intf_In_Parm     : in     Nested.Intf'Class;
                  Intf_In_Out_Parm : in out Nested.Intf'Class;
                  Int              : in     Integer);
   private
      Prot_Component : Integer;
   end POT2;

   protected body POT2 is
      entry PE21 (Prot_In_Parm     : in     POT1;
                  Prot_In_Out_Parm : in out POT1;
		  Intf_In_Parm     : in     Nested.Intf'Class;
                  Intf_In_Out_Parm : in out Nested.Intf'Class;
                  Int              : in     Integer) when True is
      begin
         case Int is
           when  1 => requeue Prot_Variable.PE11;                    -- OK.
           when  2 => requeue Prot_Constant.PE11;                    -- ERROR:
           when  3 => requeue Prot_In_Parm.PE11;                     -- ERROR:
           when  4 => requeue Prot_In_Out_Parm.PE11;                 -- OK.
           when  5 => requeue Prot_Func.PE11;                        -- ERROR:
           when  6 => requeue Prot_A_Var.all.PE11;                   -- OK.
           when  7 => requeue Prot_A_Cnst.all.PE11;                  -- ERROR:
           when  8 => requeue Prot_R_Var.POT.PE11;                   -- OK.
           when  9 => requeue Prot_R_Cnst.POT.PE11;                  -- ERROR:
           when 10 => requeue Prot_Arr_Var(1).PE11;                  -- OK.
           when 11 => requeue Prot_Arr_Cnst(1).PE11;                 -- ERROR:
           when 12 => requeue POT1'(Prot_Variable).PE11;             -- ERROR:
           when 13 => requeue POT1'(Prot_In_Out_Parm).PE11;          -- ERROR:
           when 14 => requeue Intf_In_Parm.PE12;                     -- ERROR:
           when 15 => requeue Intf_In_Out_Parm.PE12;                 -- OK.
           when others => null;
         end case;
      end PE21;
   end POT2;

   generic
      Formal_In : in POT1;
   package Gen is

      protected type POT3 is
         entry PE31 (Int : in Integer);
      private
         Prot_Component : Integer;
      end POT3;

   end Gen;

   package body Gen is

      protected body POT3 is
         entry PE31 (Int : in Integer) when True is
         begin
            case Int is
              when  1 => requeue Prot_Variable.PE11;                 -- OK.
              when  2 => requeue Prot_Constant.PE11;                 -- ERROR:
              when  3 => requeue Formal_In.PE11;                     -- ERROR:
              when others => null;
            end case;
         end PE31;
      end POT3;

   end Gen;

   package Fooey is new Gen (Prot_Func);

begin
   null;
end B954005;
