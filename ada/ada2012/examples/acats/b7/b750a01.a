-- B750A01.A
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
--     In the initialization expression of an object declaration, an expression
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
--      24 Aug 15   RLB   Corrected the qualification for Obj52.
--
--!

with F750A00;
procedure B750A01 is

    Obj01 : constant F750A00.Lim_Rec := F750A00.Func_Lim_Rec;    -- OK.
    Obj02 : constant F750A00.Lim_Rec := F750A00.Cnst_Lim_Rec;    -- ERROR:
    Obj03 : constant F750A00.Lim_Rec := F750A00.Var_Lim_Rec;     -- ERROR:
    Obj04 : constant F750A00.Lim_Rec := (F750A00.Func_Lim_Rec);  -- OK.
    Obj05 : constant F750A00.Lim_Rec := (F750A00.Cnst_Lim_Rec);  -- ERROR:
    Obj06 : constant F750A00.Lim_Rec := (F750A00.Var_Lim_Rec);   -- ERROR:
    Obj07 : constant F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Func_Lim_Rec);          -- OK.
    Obj08 : constant F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec);          -- ERROR:
    Obj09 : constant F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Var_Lim_Rec);           -- ERROR:
    Obj10 : constant F750A00.Lim_Rec :=
                F750A00.Lim_Rec(F750A00.Func_Lim_Rec);           -- ERROR:
    Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;        -- OK.
    Obj11 : constant F750A00.Lim_Rec :=  Ren01;                  -- ERROR:

    Obj12 : constant F750A00.Lim_Array := F750A00.Func_Lim_Array;-- OK.
    Obj13 : constant F750A00.Lim_Array := F750A00.Cnst_Lim_Array;-- ERROR:
    Obj14 : constant F750A00.Lim_Array := F750A00.Var_Lim_Array; -- ERROR:
    Obj15 : constant F750A00.Lim_Array :=
                F750A00.Func_Lim_Array(1..2);                    -- ERROR:
    Obj16 : constant F750A00.Lim_Rec :=
                F750A00.Func_Lim_Array(2);                       -- ERROR:
    Obj17 : constant F750A00.Lim_Rec :=
                (F750A00.Func_Lim_Array(2));                     -- ERROR:
    Obj18 : constant F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2));     -- ERROR:
    Obj19 : constant F750A00.Lim_Rec :=
                (F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2)));   -- ERROR:
    Ren11 : F750A00.Lim_Array renames F750A00.Func_Lim_Array;    -- OK.
    Obj20 : constant F750A00.Lim_Array := Ren11;                 -- ERROR:
    Obj21 : constant F750A00.Lim_Array := (Ren11);               -- ERROR:
    Obj22 : constant F750A00.Lim_Array :=
                F750A00.Lim_Array'(Ren11);                       -- ERROR:
    Obj23 : constant F750A00.Lim_Array := Obj12(2..3);           -- ERROR:
    Obj24 : constant F750A00.Lim_Rec := Obj12(2);                -- ERROR:
    Obj25 : constant F750A00.Lim_Rec := (Obj12(2));              -- ERROR:
    Obj26 : constant F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(Obj12(2));                      -- ERROR:
    Obj27 : constant F750A00.Lim_Rec :=
                (((F750A00.Lim_Rec'(Obj12(2)))));                -- ERROR:

    Obj30 : F750A00.Lim_Comp := F750A00.Func_Lim_Comp;           -- OK.
    Obj31 : F750A00.Lim_Comp := F750A00.Cnst_Lim_Comp;           -- ERROR:
    Obj32 : F750A00.Lim_Comp := F750A00.Var_Lim_Comp;            -- ERROR:
    Obj33 : F750A00.Lim_Comp := (F750A00.Func_Lim_Comp);         -- OK.
    Obj34 : F750A00.Lim_Comp := (F750A00.Cnst_Lim_Comp);         -- ERROR:
    Obj35 : F750A00.Lim_Comp := (F750A00.Var_Lim_Comp);          -- ERROR:
    Obj36 : F750A00.Lim_Comp :=
                F750A00.Lim_Comp'(F750A00.Func_Lim_Comp);        -- OK.
    Obj37 : F750A00.Lim_Comp :=
                F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp);        -- ERROR:
    Obj38 : F750A00.Lim_Comp :=
                F750A00.Lim_Comp'(F750A00.Var_Lim_Comp);         -- ERROR:
    Obj39 : F750A00.Lim_Comp :=
                F750A00.Lim_Comp(F750A00.Func_Lim_Comp);         -- ERROR:

    Obj41 : F750A00.Lim_Ext := F750A00.Func_Lim_Ext;             -- OK.
    Obj42 : F750A00.Lim_Ext := F750A00.Cnst_Lim_Ext;             -- ERROR:
    Obj43 : F750A00.Lim_Ext := F750A00.Var_Lim_Ext;              -- ERROR:
    Obj44 : F750A00.Lim_Rec := F750A00.Func_Lim_Ext.R;           -- ERROR:
    Obj45 : F750A00.Lim_Rec := (F750A00.Func_Lim_Ext.R);         -- ERROR:
    Obj46 : F750A00.Lim_Rec :=
                F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R);        -- ERROR:
    Obj47 : F750A00.Lim_Rec := Obj41.R;                          -- ERROR:
    Obj48 : F750A00.Lim_Rec := (Obj41.R);                        -- ERROR:
    Obj49 : F750A00.Lim_Rec := F750A00.Lim_Rec'(Obj41.R);        -- ERROR:
    Ren51 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;        -- OK.
    Obj50 : F750A00.Lim_Ext := Ren51;                            -- ERROR:
    Obj51 : F750A00.Lim_Ext := (Ren51);                          -- ERROR:
    Obj52 : F750A00.Lim_Ext := F750A00.Lim_Ext'(Ren51);          -- ERROR:
    Obj53 : F750A00.Lim_Tagged'Class :=
                 F750A00.Func_Lim_Tagged (True);                 -- OK.
    Obj54 : F750A00.Lim_Tagged'Class :=
                 ((F750A00.Func_Lim_Tagged (False)));            -- OK.
    Obj55 : F750A00.Lim_Tagged'Class :=
                 F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False)));           -- OK.
    Obj56 : F750A00.Lim_Tagged'Class :=
                 F750A00.Func_Lim_Tagged_Access (False).all;     -- ERROR:
    Obj57 : F750A00.Lim_Tagged'Class :=
                 (F750A00.Func_Lim_Tagged_Access (True).all);    -- ERROR:
    Obj58 : F750A00.Lim_Tagged'Class :=
                 F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all);  -- ERROR:
    Obj59 : F750A00.Lim_Tagged'Class :=
                 F750A00.Obj_Any_Tagged_Access.all;              -- ERROR:
    Obj60 : F750A00.Lim_Tagged'Class :=
                 (F750A00.Obj_Any_Tagged_Access.all);            -- ERROR:
    Obj61 : F750A00.Lim_Tagged'Class :=
                 F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all);          -- ERROR:
    Obj62 : F750A00.Lim_Rec :=
                 F750A00.Func_Lim_Tagged_Access (False).R;       -- ERROR:
    Obj63 : F750A00.Lim_Rec :=
                 F750A00.Obj_Any_Tagged_Access.R;                -- ERROR:

begin
    null;
end B750A01;
