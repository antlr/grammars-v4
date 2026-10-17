-- B750A07.A
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
--     In the return expression of an expression function,
--     an expression of a limited type cannot be anything other than an
--     aggregate, function call, or a qualified or parenthesized expression
--     whose operand would be allowed.
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
--     We will test the Ada 2012 features of conditional expressions and
--     raise expressions separately. This test was constructed from the
--     related test B750A06.
--
-- CHANGE HISTORY:
--      31 Aug 15  RLB   Created.
--      22 Jan 16  RLB   Corrected objective.
--      28 Mar 16  RLB   Added error location codes.
--      06 Feb 18  RLB   Fixed error location codes for two-line expressions.
--!

with F750A00;
procedure B750A07 is

    Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;      -- OK. {37;1}
    Ren11 : F750A00.Lim_Array renames F750A00.Func_Lim_Array;  -- OK. {39;1}
    Obj12 : constant F750A00.Lim_Array := F750A00.Func_Lim_Array;-- OK. {43;1}
    Obj41 : F750A00.Lim_Ext := F750A00.Func_Lim_Ext;           -- OK. {32;1}
    Ren51 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;      -- OK. {37;1}

    function Func01 return F750A00.Lim_Rec is
            (F750A00.Func_Lim_Rec);                            -- OK. {14;2}

    function Func02 return F750A00.Lim_Rec is
            (F750A00.Cnst_Lim_Rec);                            -- ERROR: {14;2}

    function Func03 return F750A00.Lim_Rec is
            (F750A00.Var_Lim_Rec);                             -- ERROR: {14;2}

    function Func04 return F750A00.Lim_Rec is
        ((F750A00.Func_Lim_Rec));                              -- OK. {11;3}

    function Func05 return F750A00.Lim_Rec is
        ((F750A00.Cnst_Lim_Rec));                              -- ERROR: {11;3}

    function Func06 return F750A00.Lim_Rec is
        ((F750A00.Var_Lim_Rec));                               -- ERROR: {11;3}

    function Func07 return F750A00.Lim_Rec is
        (F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));              -- OK. {10;2}

    function Func08 return F750A00.Lim_Rec is
            (F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));          -- ERROR: {31;3}

    function Func09 return F750A00.Lim_Rec is
            (F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));           -- ERROR: {31;3}

    function Func10 return F750A00.Lim_Rec is
            (F750A00.Lim_Rec(F750A00.Func_Lim_Rec));           -- ERROR: {31;3}

    function Func11 return F750A00.Lim_Rec is
            (Ren01);                                           -- ERROR: {14;2}

    function Func12 return F750A00.Lim_Array is
        (F750A00.Func_Lim_Array);                              -- OK. {10;2)

    function Func13 return F750A00.Lim_Array is
        (F750A00.Cnst_Lim_Array);                              -- ERROR: {10;2}

    function Func14 return F750A00.Lim_Array is
        (F750A00.Var_Lim_Array);                               -- ERROR: {10;2}

    function Func15 return F750A00.Lim_Array is
        (F750A00.Func_Lim_Array(1..2));                        -- ERROR: {10;2}

    function Func16 return F750A00.Lim_Rec is
            (F750A00.Func_Lim_Array(2));                       -- ERROR: {14;2}

    function Func17 return F750A00.Lim_Rec is
            ((F750A00.Func_Lim_Array(2)));                     -- ERROR: {15;3}

    function Func18 return F750A00.Lim_Rec is
            (F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2)));     -- ERROR: {31;3}

    function Func19 return F750A00.Lim_Rec is
            ((F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))));   -- ERROR: {32;4}

    function Func20 return F750A00.Lim_Array is
            (Ren11);                                           -- ERROR: {14;2}

    function Func21 return F750A00.Lim_Array is
            ((Ren11));                                         -- ERROR: {15;3}

    function Func22 return F750A00.Lim_Array is
            (F750A00.Lim_Array'(Ren11));                       -- ERROR: {33;3}

    function Func23 return F750A00.Lim_Array is
            (Obj12(2..3));                                     -- ERROR: {14;2}

    function Func24 return F750A00.Lim_Rec is
            (Obj12(2));                                        -- ERROR: {14;2}

    function Func25 return F750A00.Lim_Rec is
            ((Obj12(2)));                                      -- ERROR: {15;3}

    function Func26 return F750A00.Lim_Rec is
            (F750A00.Lim_Rec'(Obj12(2)));                      -- ERROR: {31;3}

    function Func27 return F750A00.Lim_Rec is
            ((((F750A00.Lim_Rec'(Obj12(2))))));                -- ERROR: {34;6}

    function Func30 return F750A00.Lim_Comp is
            (F750A00.Func_Lim_Comp);                           -- OK. {14;2}

    function Func31 return F750A00.Lim_Comp is
            (F750A00.Cnst_Lim_Comp);                           -- ERROR: {14;2}

    function Func32 return F750A00.Lim_Comp is
            (F750A00.Var_Lim_Comp);                            -- ERROR: {14;2}

    function Func33 return F750A00.Lim_Comp is
            ((F750A00.Func_Lim_Comp));                         -- OK. {15;3}

    function Func34 return F750A00.Lim_Comp is
            ((F750A00.Cnst_Lim_Comp));                         -- ERROR: {15;3}

    function Func35 return F750A00.Lim_Comp is
            ((F750A00.Var_Lim_Comp));                          -- ERROR: {15;3}

    function Func36 return F750A00.Lim_Comp is
            (F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));        -- OK. {32;3}

    function Func37 return F750A00.Lim_Comp is
            (F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));        -- ERROR: {32;3}

    function Func38 return F750A00.Lim_Comp is
            (F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));         -- ERROR: {32;3}

    function Func39 return F750A00.Lim_Comp is
            (F750A00.Lim_Comp(F750A00.Func_Lim_Comp));         -- ERROR: {32;3}

    function Func41 return F750A00.Lim_Ext is
            (F750A00.Func_Lim_Ext);                            -- OK. {14;2}

    function Func42 return F750A00.Lim_Ext is
            (F750A00.Cnst_Lim_Ext);                            -- ERROR: {14;2}

    function Func43 return F750A00.Lim_Ext is
            (F750A00.Var_Lim_Ext);                             -- ERROR: {14;2}

    function Func44 return F750A00.Lim_Rec is
            (F750A00.Func_Lim_Ext.R);                          -- ERROR: {14;2}

    function Func45 return F750A00.Lim_Rec is
            ((F750A00.Func_Lim_Ext.R));                        -- ERROR: {15;3}

    function Func46 return F750A00.Lim_Rec is
            (F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));        -- ERROR: {31;3}

    function Func47 return F750A00.Lim_Rec is
            (Obj41.R);                                         -- ERROR: {14;2}

    function Func48 return F750A00.Lim_Rec is
            ((Obj41.R));                                       -- ERROR: {15;3}

    function Func49 return F750A00.Lim_Rec is
            (F750A00.Lim_Rec'(Obj41.R));                       -- ERROR: {31;3}

    function Func50 return F750A00.Lim_Ext is
            (Ren51);                                           -- ERROR: {14;2}

    function Func51 return F750A00.Lim_Ext is
            ((Ren51));                                         -- ERROR: {15;3}

    function Func52 return F750A00.Lim_Ext is
            (F750A00.Lim_Ext'(Ren51));                         -- ERROR: {31;3}

    function Func53 return F750A00.Lim_Tagged'Class is
            (F750A00.Func_Lim_Tagged (True));                  -- OK. {14;2}

    function Func54 return F750A00.Lim_Tagged'Class is
            (((F750A00.Func_Lim_Tagged (False))));             -- OK. {16;4}

    function Func55 return F750A00.Lim_Tagged'Class is
        (F750A00.Lim_Tagged'Class'((
                   F750A00.Func_Lim_Tagged (False))));         -- OK. {1:10;4}

    function Func56 return F750A00.Lim_Tagged'Class is
            (F750A00.Func_Lim_Tagged_Access (False).all);      -- ERROR: {14;2}

    function Func57 return F750A00.Lim_Tagged'Class is
            ((F750A00.Func_Lim_Tagged_Access (True).all));     -- ERROR: {15;3}

    function Func58 return F750A00.Lim_Tagged'Class is
        (F750A00.Lim_Tagged'Class'(
                 F750A00.Func_Lim_Tagged_Access (True).all));-- ERROR: {1:10;3}

    function Func59 return F750A00.Lim_Tagged'Class is
            (F750A00.Obj_Any_Tagged_Access.all);               -- ERROR: {14;2}

    function Func60 return F750A00.Lim_Tagged'Class is
            ((F750A00.Obj_Any_Tagged_Access.all));             -- ERROR: {15;3}

    function Func61 return F750A00.Lim_Tagged'Class is
            (F750A00.Lim_Tagged'Class'(
                   F750A00.Obj_Any_Tagged_Access.all));      -- ERROR: {1:14;3}

    function Func62 return F750A00.Lim_Rec is
            (F750A00.Func_Lim_Tagged_Access (False).R);        -- ERROR: {14;2}

    function Func63 return F750A00.Lim_Rec is
        (F750A00.Obj_Any_Tagged_Access.R);                     -- ERROR: {10;2}

begin
    null;
end B750A07;
