-- B840003.A
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
-- OBJECTIVE:
--     Check that a use all type clause does not make non-primitive subprograms
--     declared in the package where the named subtype is declared directly
--     visible.
--
--     Check that a use all type clause of a specific tagged type T does not
--     make operations of T'Class directly visible unless they are declared
--     in the same package as T or an ancestor of T.
--
-- CHANGE HISTORY:
--      15 Apr 15   RLB   Created test, based on C840002.
--!

procedure B840003 is
begin

   declare
      package Pkg_1 is
         type Enum is (Aaa, Bbb, Ccc);
         procedure Prim_Proc (X : Enum := Aaa);
         function Prim_Func return Enum;

         package Nested is
            procedure Nonprim_Proc (X : Enum := Bbb);
            function Nonprim_Func return Enum;
         end Nested;

      end Pkg_1;

      package body Pkg_1 is
         procedure Prim_Proc (X : Enum := Aaa) is null;
         function Prim_Func return Enum is (Bbb);

         package body Nested is
            procedure Nonprim_Proc (X : Enum := Bbb) is null;
            function Nonprim_Func return Enum is (Ccc);
         end Nested;
      end Pkg_1;

   begin
      declare
         use all type Pkg_1.Enum;
      begin
         if Prim_Func /= Bbb then                                  -- OK.
            null;
         end if;
         Nonprim_Proc (Ccc);                                       -- ERROR:
         Nonprim_Proc (Aaa);                                       -- ERROR:
         Prim_Proc;                                                -- OK.
      end;
   end;

   declare
      package T1_Pkg is
         type T1 is tagged null record;
         procedure Prim_Proc (X : T1);
         function  Prim_Func return T1;
         T1_Var : T1;
         procedure Cw_Operand (X : T1'Class := T1_Var);
      end T1_Pkg;

      package T2_Pkg is
         type T2 is new T1_Pkg.T1 with null record;
         T2_Var : T2;
         package Nested is
            procedure Nested_Cw_Operand (X : T2'Class := T2_Var);
            procedure Nonprim_Proc (X : T2);
         end Nested;
      end T2_Pkg;

      package T3_Pkg is
         type T3 is new T2_Pkg.T2 with null record;
         T3_Var : T3;
      end T3_Pkg;

      package Unrelated is
         procedure Another_Cw_Operand (X : T2_Pkg.T2'Class := T2_Pkg.T2_Var);
      end Unrelated;

      package body T1_Pkg is
         procedure Prim_Proc (X : T1) is null;
         function  Prim_Func return T1 is (T1_Var);
         procedure Cw_Operand (X : T1'Class := T1_Var) is null;
      end T1_Pkg;

      package body T2_Pkg is
         package body Nested is
            procedure Nested_Cw_Operand (X : T2'Class := T2_Var) is null;
            procedure Nonprim_Proc (X : T2) is null;
         end Nested;
      end T2_Pkg;

      package body T3_Pkg is
      end T3_Pkg;

      package body Unrelated is
         procedure Another_Cw_Operand (X : T2_Pkg.T2'Class := T2_Pkg.T2_Var)
            is null;
      end Unrelated;

   begin
      declare
         use all type T2_Pkg.T2;

         procedure Ren1 (X : T1_Pkg.T1'Class) renames Cw_Operand;  -- OK.
             -- preceding "use all type" makes T1_Pkg.Cw_Operand use-visible
             -- because T1 is an ancestor of T2.

         procedure Ren2 (X : T2_Pkg.T2'Class) renames
             Nested_Cw_Operand;                                    -- ERROR:

      begin
         if Prim_Func /= Prim_Func then                            -- OK.
             null;
         end if;
         Prim_Proc (T2_Pkg.T2'Class (T3_Pkg.T3_Var));              -- OK.
         Nonprim_Proc (T2_Pkg.T2_Var);                             -- ERROR:
         Nested_Cw_Operand (T2_Pkg.T2_Var);                        -- ERROR:
         Another_Cw_Operand (T2_Pkg.T2_Var);                       -- ERROR:
         Cw_Operand (T2_Pkg.T2_Var);                               -- OK.
      end;

      declare
         use all type T3_Pkg.T3;

         procedure Ren1 (X : T1_Pkg.T1'Class) renames Cw_Operand;  -- OK.
             -- preceding "use all type" makes T1_Pkg.Cw_Operand use-visible
             -- because T1 is an ancestor of T2.

         procedure Ren2 (X : T2_Pkg.T2'Class) renames
             Nested_Cw_Operand;                                    -- ERROR:

      begin
         if Prim_Func /= Prim_Func then                            -- OK.
             null;
         end if;
         Prim_Proc (T3_Pkg.T3'Class (T3_Pkg.T3_Var));              -- OK.
         Nested_Cw_Operand (T3_Pkg.T3_Var);                        -- ERROR:
         Another_Cw_Operand (T3_Pkg.T3_Var);                       -- ERROR:
         Cw_Operand (T3_Pkg.T3_Var);                               -- OK.
      end;
   end;

end B840003;
