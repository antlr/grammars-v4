-- B750A04.A
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
--     In the qualified expression of an initialized allocator, an expression
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
--     We also try various combinations of parenthisized expressions of the
--     above. (We don't specifically try qualified expressions in this test,
--     since essentially the entire test is trying that; it is always present
--     in the syntax of the allocator.)
--
--     This is the Ada 2005 test (an Ada 2012 would include conditional
--     expressions and raise expressions; those will be tested separately).
--     It was constructed from existing test B750A01.
--
-- CHANGE HISTORY:
--      24 Aug 15  RLB   Created.
--
--!

with F750A00;
procedure B750A04 is

    type Acc_Lim_Rec is access all F750A00.Lim_Rec;
    type Acc_Lim_Comp is access all F750A00.Lim_Comp;

    Obj01 : Acc_Lim_Rec :=
                new F750A00.Lim_Rec'(F750A00.Func_Lim_Rec);      -- OK.
    Obj02 : Acc_Lim_Rec :=
                new F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec);      -- ERROR:
    Obj03 : Acc_Lim_Rec :=
                new F750A00.Lim_Rec'(F750A00.Var_Lim_Rec);       -- ERROR:
    Obj04 : Acc_Lim_Rec :=
                new F750A00.Lim_Rec'((F750A00.Func_Lim_Rec));    -- OK.
    Obj05 : Acc_Lim_Rec :=
                new F750A00.Lim_Rec'((F750A00.Cnst_Lim_Rec));    -- ERROR:
    Obj06 : Acc_Lim_Rec :=
                 new F750A00.Lim_Rec'((F750A00.Var_Lim_Rec));    -- ERROR:
    Obj07 : Acc_Lim_Rec := Obj01;                                -- OK.
    Obj08 : Acc_Lim_Rec :=
                new F750A00.Lim_Rec'(Obj01.all);                 -- ERROR:

    Obj10 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
                F750A00.Lim_Rec(F750A00.Func_Lim_Rec));          -- ERROR:
    Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;        -- OK.
    Obj11 : Acc_Lim_Rec := new F750A00.Lim_Rec'(Ren01);          -- ERROR:

    Obj12 : access F750A00.Lim_Array :=
                new F750A00.Lim_Array'(F750A00.Func_Lim_Array);  -- OK.
    Obj13 : access F750A00.Lim_Array :=
                new F750A00.Lim_Array'(F750A00.Cnst_Lim_Array);  -- ERROR:
    Obj14 : access F750A00.Lim_Array :=
                new F750A00.Lim_Array'(F750A00.Var_Lim_Array);   -- ERROR:
    Obj15 : access F750A00.Lim_Array := new F750A00.Lim_Array'(
                F750A00.Func_Lim_Array(1..2));                   -- ERROR:
    Obj16 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
                F750A00.Func_Lim_Array(2));                      -- ERROR:
    Obj17 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
                (F750A00.Func_Lim_Array(2)));                    -- ERROR:

    Ren11 : F750A00.Lim_Array renames F750A00.Func_Lim_Array;    -- OK.
    Obj20 : access F750A00.Lim_Array :=
                new F750A00.Lim_Array'(Ren11);                   -- ERROR:
    Obj21 : access F750A00.Lim_Array :=
                new F750A00.Lim_Array'((Ren11));                 -- ERROR:
    Obj23 : access F750A00.Lim_Array :=
                new F750A00.Lim_Array'(Obj12(2..3));             -- ERROR:
    Obj24 : Acc_Lim_Rec := new F750A00.Lim_Rec'(Obj12(2));       -- ERROR:
    Obj25 : Acc_Lim_Rec := new F750A00.Lim_Rec'((Obj12(2)));     -- ERROR:
    Obj26 : Acc_Lim_Rec := new F750A00.Lim_Rec'(Ren11(2));       -- ERROR:
    Obj27 : Acc_Lim_Rec := new F750A00.Lim_Rec'((Ren11(2)));     -- ERROR:

    Obj30 : Acc_Lim_Comp :=
                  new F750A00.Lim_Comp'(F750A00.Func_Lim_Comp);  -- OK.
    Obj31 : Acc_Lim_Comp :=
                  new F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp);  -- ERROR:
    Obj32 : Acc_Lim_Comp :=
                  new F750A00.Lim_Comp'(F750A00.Var_Lim_Comp);   -- ERROR:
    Obj33 : Acc_Lim_Comp :=
                  new F750A00.Lim_Comp'((F750A00.Func_Lim_Comp));-- OK.
    Obj34 : Acc_Lim_Comp :=
                  new F750A00.Lim_Comp'((F750A00.Cnst_Lim_Comp));-- ERROR:
    Obj35 : Acc_Lim_Comp :=
                  new F750A00.Lim_Comp'((F750A00.Var_Lim_Comp)); -- ERROR:
    Obj39 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
                F750A00.Lim_Comp(F750A00.Func_Lim_Comp));        -- ERROR:

    Obj41 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
                              F750A00.Func_Lim_Ext);             -- OK.
    Obj42 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
                              F750A00.Cnst_Lim_Ext);             -- ERROR:
    Obj43 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
                              F750A00.Var_Lim_Ext);              -- ERROR:
    Obj44 : Acc_Lim_Rec :=
                new F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R);    -- ERROR:
    Obj45 : Acc_Lim_Rec :=
                new F750A00.Lim_Rec'((F750A00.Func_Lim_Ext.R));  -- ERROR:
    Obj46 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
                F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));       -- ERROR:
    Obj47 : Acc_Lim_Rec := new F750A00.Lim_Rec'(Obj41.R);        -- ERROR:
    Obj48 : Acc_Lim_Rec := new F750A00.Lim_Rec'((Obj41.R));      -- ERROR:

    Ren51 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;        -- OK.
    Obj50 : access F750A00.Lim_Ext :=
                               new F750A00.Lim_Ext'(Ren51);      -- ERROR:
    Obj51 : access F750A00.Lim_Ext :=
                               new F750A00.Lim_Ext'((Ren51));    -- ERROR:
    Obj52 : Acc_Lim_Rec := new F750A00.Lim_Rec'(Ren51.R);        -- ERROR:
    Obj53 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
                 F750A00.Func_Lim_Tagged (True));                -- OK.
    Obj54 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
                 ((F750A00.Func_Lim_Tagged (False))));           -- OK.
    Obj56 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
                 F750A00.Func_Lim_Tagged_Access (False).all);    -- ERROR:
    Obj57 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
                 (F750A00.Func_Lim_Tagged_Access (True).all));   -- ERROR:
    Obj59 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
                 F750A00.Obj_Any_Tagged_Access.all);             -- ERROR:
    Obj60 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
                 (F750A00.Obj_Any_Tagged_Access.all));           -- ERROR:
    Obj61 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
                 F750A00.Func_Lim_Tagged_Access (False).R);      -- ERROR:
    Obj62 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
                 F750A00.Obj_Any_Tagged_Access.R);               -- ERROR:

begin
    null;
end B750A04;
