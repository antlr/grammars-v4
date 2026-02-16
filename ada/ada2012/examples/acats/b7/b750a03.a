-- B750A03.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--     In the expression of a record component association, an expression
--     of a limited type cannot be anything other than an aggregate, function
--     call, or a qualified or parenthesized expression whose operand would
--     be allowed.
--
-- TEST DESCRIPTION:
--     We try every kind of name of an entity with a limited type:
--     constant and variable names (including those that are dereferenced,
--     selected, indexed, and sliced), function calls that are dereferenced,
--     selected, indexed, and sliced, object renames of function calls,
--     and type conversions to limited types.
--     We also try various combinations of qualified and parenthisized
--     expressions of the above.
--
-- CHANGE HISTORY:
--      03 May 07   RLB   Created from existing test.
--      17 Aug 07   RLB   Fixed type error.
--
--!

with F750A00;
procedure B750A03 is

   Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;         -- OK.
   Ren11 : F750A00.Lim_Array renames F750A00.Func_Lim_Array;     -- OK.
   Ren51 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;         -- OK.

   Obj1 : constant F750A00.Lim_Array := F750A00.Func_Lim_Array;  -- OK.
   Obj2 : F750A00.Lim_Ext := F750A00.Func_Lim_Ext;               -- OK.


   Agg01 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Func_Lim_Rec);                       -- OK.
   Agg02 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Cnst_Lim_Rec);                       -- ERROR:
   Agg03 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Var_Lim_Rec);                        -- ERROR:
   Agg04 : F750A00.Lim_Tagged :=
      (N => 1, R => (F750A00.Func_Lim_Rec));                     -- OK.
   Agg05 : F750A00.Lim_Tagged :=
      (N => 1, R => (F750A00.Cnst_Lim_Rec));                     -- ERROR:
   Agg06 : F750A00.Lim_Tagged :=
      (N => 1, R => (F750A00.Var_Lim_Rec));                      -- ERROR:
   Agg07 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));     -- OK.
   Agg08 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));     -- ERROR:
   Agg09 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));      -- ERROR:
   Agg10 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Lim_Rec(F750A00.Func_Lim_Rec));      -- ERROR:
   Agg11 : F750A00.Lim_Tagged :=
      (N => 1, R => Ren01);                                      -- ERROR:
   Agg12 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Func_Lim_Array(2));                  -- ERROR:
   Agg13 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Func_Lim_Array(2));                  -- ERROR:
   Agg14 : F750A00.Lim_Tagged :=
      (N => 1, R => (F750A00.Func_Lim_Array(2)));                -- ERROR:
   Agg15 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2)));-- ERROR:
   Agg16 : F750A00.Lim_Tagged :=
      (N => 1, R =>
         (F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))));         -- ERROR:
   Agg17 : F750A00.Lim_Tagged :=
      (N => 1, R => Obj1(2));                                    -- ERROR:
   Agg18 : F750A00.Lim_Tagged :=
      (N => 1, R => (Obj1(2)));                                  -- ERROR:
   Agg19 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Lim_Rec'(Obj1(2)));                  -- ERROR:
   Agg20 : F750A00.Lim_Tagged :=
      (N => 1, R => (((F750A00.Lim_Rec'(Obj1(2))))));            -- ERROR:
   Agg21 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Func_Lim_Ext.R);                     -- ERROR:
   Agg22 : F750A00.Lim_Tagged :=
      (N => 1, R => (F750A00.Func_Lim_Ext.R));                   -- ERROR:
   Agg23 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));   -- ERROR:
   Agg24 : F750A00.Lim_Tagged :=
      (N => 1, R => Obj2.R);                                     -- ERROR:
   Agg25 : F750A00.Lim_Tagged :=
      (N => 1, R => (Obj2.R));                                   -- ERROR:
   Agg26 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Lim_Rec'(Obj2.R));                   -- ERROR:
   Agg27 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Func_Lim_Tagged_Access (False).R);   -- ERROR:
   Agg28 : F750A00.Lim_Tagged :=
      (N => 1, R => F750A00.Obj_Any_Tagged_Access.R);            -- ERROR:

   type Test_Rec is record
      N : Natural;
      Arr : F750A00.Short_Lim_Array;
   end record;

   Agg29 : Test_Rec :=
      (N => 1, Arr => F750A00.Func_Lim_Array);                   -- OK.
   Agg30 : Test_Rec :=
      (N => 1, Arr => F750A00.Cnst_Lim_Array);                   -- ERROR:
   Agg31 : Test_Rec :=
      (N => 1, Arr => F750A00.Var_Lim_Array);                    -- ERROR:
   Agg32 : Test_Rec :=
      (N => 1, Arr => F750A00.Func_Lim_Array(1..3));             -- ERROR:
   Agg33 : Test_Rec :=
      (N => 1, Arr => Ren11);                                    -- ERROR:
   Agg34 : Test_Rec :=
      (N => 1, Arr => (Ren11));                                  -- ERROR:
   Agg35 : Test_Rec :=
      (N => 1, Arr => F750A00.Lim_Array'(Ren11));                -- ERROR:
   Agg36 : Test_Rec :=
      (N => 1, Arr => Obj1(1..3));                               -- ERROR:

   type Test_Rec2 is record
      N : Natural;
      Ext : F750A00.Lim_Ext;
   end record;

   Agg37 : Test_Rec2 :=
      (N => 1, Ext => F750A00.Func_Lim_Ext);                     -- OK.
   Agg38 : Test_Rec2 :=
      (N => 1, Ext => F750A00.Cnst_Lim_Ext);                     -- ERROR:
   Agg39 : Test_Rec2 :=
      (N => 1, Ext => F750A00.Var_Lim_Ext);                      -- ERROR:
   Agg40 : Test_Rec2 :=
      (N => 1, Ext => Ren51);                                    -- ERROR:
   Agg41 : Test_Rec2 :=
      (N => 1, Ext => (Ren51));                                  -- ERROR:
   Agg42 : Test_Rec2 :=
      (N => 1, Ext => F750A00.Lim_Ext'(Ren51));                  -- ERROR:
   Agg43 : Test_Rec2 :=
      (N => 1, Ext => F750A00.Func_Lim_Ext_Access.all);          -- ERROR:
   Agg44 : Test_Rec2 :=
      (N => 1, Ext => (F750A00.Func_Lim_Ext_Access.all));        -- ERROR:
   Agg45 : Test_Rec2 :=
      (N => 1, Ext => (F750A00.Func_Lim_Ext_Access.all));        -- ERROR:
   Agg46 : Test_Rec2 :=
      (N => 1, Ext => F750A00.Obj_Lim_Ext_Access.all);           -- ERROR:
   Agg47 : Test_Rec2 :=
      (N => 1, Ext => (F750A00.Obj_Lim_Ext_Access.all));         -- ERROR:
   Agg48 : Test_Rec2 :=
      (N => 1, Ext => (F750A00.Obj_Lim_Ext_Access.all));         -- ERROR:

begin
   null;
end B750A03;
