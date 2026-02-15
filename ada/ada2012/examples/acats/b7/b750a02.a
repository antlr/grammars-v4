-- B750A02.A
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
--     In the default expression of a component declaration, an expression
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
--      02 May 07   RLB   Created.
--      24 Aug 15   RLB   Corrected the qualification for Comp52.
--
--!

with F750A00;
procedure B750A02 is

   Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;         -- OK.
   Ren11 : F750A00.Lim_Array renames F750A00.Func_Lim_Array;     -- OK.
   Ren51 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;         -- OK.

   Obj1 : constant F750A00.Lim_Array := F750A00.Func_Lim_Array;  -- OK.
   Obj2 : F750A00.Lim_Ext := F750A00.Func_Lim_Ext;               -- OK.

   type Ginormous is record
      Comp01 : F750A00.Lim_Rec := F750A00.Func_Lim_Rec;          -- OK.
      Comp02 : F750A00.Lim_Rec := F750A00.Cnst_Lim_Rec;          -- ERROR:
      Comp03 : F750A00.Lim_Rec := F750A00.Var_Lim_Rec;           -- ERROR:
      Comp04 : F750A00.Lim_Rec := (F750A00.Func_Lim_Rec);        -- OK.
      Comp05 : F750A00.Lim_Rec := (F750A00.Cnst_Lim_Rec);        -- ERROR:
      Comp06 : F750A00.Lim_Rec := (F750A00.Var_Lim_Rec);         -- ERROR:
      Comp07 : F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Func_Lim_Rec);          -- OK.
      Comp08 : F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec);          -- ERROR:
      Comp09 : F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Var_Lim_Rec);           -- ERROR:
      Comp10 : F750A00.Lim_Rec :=
                F750A00.Lim_Rec(F750A00.Func_Lim_Rec);           -- ERROR:
      Comp11 : F750A00.Lim_Rec := Ren01;                         -- ERROR:

      Comp12 : F750A00.Short_Lim_Array := F750A00.Func_Lim_Array;-- OK.
      Comp13 : F750A00.Short_Lim_Array := F750A00.Cnst_Lim_Array;-- ERROR:
      Comp14 : F750A00.Short_Lim_Array := F750A00.Var_Lim_Array; -- ERROR:
      Comp15 : F750A00.Lim_Array(2..3) :=
                F750A00.Func_Lim_Array(1..2);                    -- ERROR:
      Comp16 : F750A00.Lim_Rec := F750A00.Func_Lim_Array(2);     -- ERROR:
      Comp17 : F750A00.Lim_Rec := (F750A00.Func_Lim_Array(2));   -- ERROR:
      Comp18 : F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2));     -- ERROR:
      Comp19 : F750A00.Lim_Rec :=
                (F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2)));   -- ERROR:
      Comp20 : F750A00.Short_Lim_Array := Ren11;                 -- ERROR:
      Comp21 : F750A00.Short_Lim_Array := (Ren11);               -- ERROR:
      Comp22 : F750A00.Short_Lim_Array :=
                F750A00.Lim_Array'(Ren11);                       -- ERROR:
      Comp23 : F750A00.Short_Lim_Array := Obj1(1..3);            -- ERROR:
      Comp24 : F750A00.Lim_Rec := Obj1(2);                       -- ERROR:
      Comp25 : F750A00.Lim_Rec := (Obj1(2));                     -- ERROR:
      Comp26 : F750A00.Lim_Rec := F750A00.Lim_Rec'(Obj1(2));     -- ERROR:
      Comp27 : F750A00.Lim_Rec :=
                (((F750A00.Lim_Rec'(Obj1(2)))));                 -- ERROR:

      Comp30 : F750A00.Lim_Comp := F750A00.Func_Lim_Comp;        -- OK.
      Comp31 : F750A00.Lim_Comp := F750A00.Cnst_Lim_Comp;        -- ERROR:
      Comp32 : F750A00.Lim_Comp := F750A00.Var_Lim_Comp;         -- ERROR:
      Comp33 : F750A00.Lim_Comp := (F750A00.Func_Lim_Comp);      -- OK.
      Comp34 : F750A00.Lim_Comp := (F750A00.Cnst_Lim_Comp);      -- ERROR:
      Comp35 : F750A00.Lim_Comp := (F750A00.Var_Lim_Comp);       -- ERROR:
      Comp36 : F750A00.Lim_Comp :=
                F750A00.Lim_Comp'(F750A00.Func_Lim_Comp);        -- OK.
      Comp37 : F750A00.Lim_Comp :=
                F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp);        -- ERROR:
      Comp38 : F750A00.Lim_Comp :=
                F750A00.Lim_Comp'(F750A00.Var_Lim_Comp);         -- ERROR:
      Comp39 : F750A00.Lim_Comp :=
                F750A00.Lim_Comp(F750A00.Func_Lim_Comp);         -- ERROR:

      Comp41 : F750A00.Lim_Ext := F750A00.Func_Lim_Ext;          -- OK.
      Comp42 : F750A00.Lim_Ext := F750A00.Cnst_Lim_Ext;          -- ERROR:
      Comp43 : F750A00.Lim_Ext := F750A00.Var_Lim_Ext;           -- ERROR:
      Comp44 : F750A00.Lim_Rec := F750A00.Func_Lim_Ext.R;        -- ERROR:
      Comp45 : F750A00.Lim_Rec := (F750A00.Func_Lim_Ext.R);      -- ERROR:
      Comp46 : F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R);        -- ERROR:
      Comp47 : F750A00.Lim_Rec := Obj2.R;                        -- ERROR:
      Comp48 : F750A00.Lim_Rec := (Obj2.R);                      -- ERROR:
      Comp49 : F750A00.Lim_Rec := F750A00.Lim_Rec'(Obj2.R);      -- ERROR:
      Comp50 : F750A00.Lim_Ext := Ren51;                         -- ERROR:
      Comp51 : F750A00.Lim_Ext := (Ren51);                       -- ERROR:
      Comp52 : F750A00.Lim_Ext := F750A00.Lim_Ext'(Ren51);       -- ERROR:
      Comp53 : F750A00.Lim_Tagged := F750A00.Func_New_One;       -- OK.
      Comp54 : F750A00.Lim_Tagged := ((F750A00.Func_New_One));   -- OK.
      Comp55 : F750A00.Lim_Tagged :=
                 F750A00.Lim_Tagged'((F750A00.Func_New_One));    -- OK.
      Comp56 : F750A00.Lim_Ext := F750A00.Func_Lim_Ext_Access.all;-- ERROR:
      Comp57 : F750A00.Lim_Ext :=
                 (F750A00.Func_Lim_Ext_Access.all);              -- ERROR:
      Comp58 : F750A00.Lim_Ext := F750A00.Lim_Ext'
                 (F750A00.Func_Lim_Ext_Access.all);              -- ERROR:
      Comp59 : F750A00.Lim_Ext := F750A00.Obj_Lim_Ext_Access.all;-- ERROR:
      Comp60 : F750A00.Lim_Ext :=
                 (F750A00.Obj_Lim_Ext_Access.all);               -- ERROR:
      Comp61 : F750A00.Lim_Ext := F750A00.Lim_Ext'
                 (F750A00.Obj_Lim_Ext_Access.all);               -- ERROR:
      Comp62 : F750A00.Lim_Rec :=
                 F750A00.Func_Lim_Tagged_Access (False).R;       -- ERROR:
      Comp63 : F750A00.Lim_Rec :=
                 F750A00.Obj_Any_Tagged_Access.R;                -- ERROR:
   end record;

begin
   null;
end B750A02;
