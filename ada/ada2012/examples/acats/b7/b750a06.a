-- B750A06.A
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
--     In the expression of a simple return statement,
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
--     This is the Ada 2005 test (an Ada 2012 test would include conditional
--     expressions and raise expressions; those will be tested separately).
--     This test was constructed from the related test B750A05.
--
--
-- CHANGE HISTORY:
--      31 Aug 15  RLB   Created.
--
--!

with F750A00;
procedure B750A06 is

    Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;        -- OK.
    Ren11 : F750A00.Lim_Array renames F750A00.Func_Lim_Array;    -- OK.
    Obj12 : constant F750A00.Lim_Array := F750A00.Func_Lim_Array;-- OK.
    Obj41 : F750A00.Lim_Ext := F750A00.Func_Lim_Ext;             -- OK.
    Ren51 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;        -- OK.

    function Func01 return F750A00.Lim_Rec is
    begin
       return F750A00.Func_Lim_Rec;                              -- OK.
    end Func01;

    function Func02 return F750A00.Lim_Rec is
    begin
       return F750A00.Cnst_Lim_Rec;                              -- ERROR:
    end Func02;

    function Func03 return F750A00.Lim_Rec is
    begin
       return F750A00.Var_Lim_Rec;                               -- ERROR:
    end Func03;

    function Func04 return F750A00.Lim_Rec is
    begin
       return (F750A00.Func_Lim_Rec);                            -- OK.
    end Func04;

    function Func05 return F750A00.Lim_Rec is
    begin
       return (F750A00.Cnst_Lim_Rec);                            -- ERROR:
    end Func05;

    function Func06 return F750A00.Lim_Rec is
    begin
       return (F750A00.Var_Lim_Rec);                             -- ERROR:
    end Func06;

    function Func07 return F750A00.Lim_Rec is
    begin
       return F750A00.Lim_Rec'(F750A00.Func_Lim_Rec);            -- OK.
    end Func07;

    function Func08 return F750A00.Lim_Rec is
    begin
       return F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec);            -- ERROR:
    end Func08;

    function Func09 return F750A00.Lim_Rec is
    begin
       return F750A00.Lim_Rec'(F750A00.Var_Lim_Rec);             -- ERROR:
    end Func09;

    function Func10 return F750A00.Lim_Rec is
    begin
       return F750A00.Lim_Rec(F750A00.Func_Lim_Rec);             -- ERROR:
    end Func10;

    function Func11 return F750A00.Lim_Rec is
    begin
       return Ren01;                                             -- ERROR:
    end Func11;

    function Func12 return F750A00.Lim_Array is
    begin
       return F750A00.Func_Lim_Array;                            -- OK.
    end Func12;

    function Func13 return F750A00.Lim_Array is
    begin
       return F750A00.Cnst_Lim_Array;                            -- ERROR:
    end Func13;

    function Func14 return F750A00.Lim_Array is
    begin
       return F750A00.Var_Lim_Array;                             -- ERROR:
    end Func14;

    function Func15 return F750A00.Lim_Array is
    begin
       return F750A00.Func_Lim_Array(1..2);                      -- ERROR:
    end Func15;

    function Func16 return F750A00.Lim_Rec is
    begin
       return F750A00.Func_Lim_Array(2);                         -- ERROR:
    end Func16;

    function Func17 return F750A00.Lim_Rec is
    begin
       return (F750A00.Func_Lim_Array(2));                       -- ERROR:
    end Func17;

    function Func18 return F750A00.Lim_Rec is
    begin
       return F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2));       -- ERROR:
    end Func18;

    function Func19 return F750A00.Lim_Rec is
    begin
       return (F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2)));     -- ERROR:
    end Func19;

    function Func20 return F750A00.Lim_Array is
    begin
       return Ren11;                                             -- ERROR:
    end Func20;

    function Func21 return F750A00.Lim_Array is
    begin
       return (Ren11);                                           -- ERROR:
    end Func21;

    function Func22 return F750A00.Lim_Array is
    begin
       return F750A00.Lim_Array'(Ren11);                         -- ERROR:
    end Func22;

    function Func23 return F750A00.Lim_Array is
    begin
       return Obj12(2..3);                                       -- ERROR:
    end Func23;

    function Func24 return F750A00.Lim_Rec is
    begin
       return Obj12(2);                                          -- ERROR:
    end Func24;

    function Func25 return F750A00.Lim_Rec is
    begin
       return (Obj12(2));                                        -- ERROR:
    end Func25;

    function Func26 return F750A00.Lim_Rec is
    begin
       return F750A00.Lim_Rec'(Obj12(2));                        -- ERROR:
    end Func26;

    function Func27 return F750A00.Lim_Rec is
    begin
       return (((F750A00.Lim_Rec'(Obj12(2)))));                  -- ERROR:
    end Func27;

    function Func30 return F750A00.Lim_Comp is
    begin
       return F750A00.Func_Lim_Comp;                             -- OK.
    end Func30;

    function Func31 return F750A00.Lim_Comp is
    begin
       return F750A00.Cnst_Lim_Comp;                             -- ERROR:
    end Func31;

    function Func32 return F750A00.Lim_Comp is
    begin
       return F750A00.Var_Lim_Comp;                              -- ERROR:
    end Func32;

    function Func33 return F750A00.Lim_Comp is
    begin
       return (F750A00.Func_Lim_Comp);                           -- OK.
    end Func33;

    function Func34 return F750A00.Lim_Comp is
    begin
       return (F750A00.Cnst_Lim_Comp);                           -- ERROR:
    end Func34;

    function Func35 return F750A00.Lim_Comp is
    begin
       return (F750A00.Var_Lim_Comp);                            -- ERROR:
    end Func35;

    function Func36 return F750A00.Lim_Comp is
    begin
       return F750A00.Lim_Comp'(F750A00.Func_Lim_Comp);          -- OK.
    end Func36;

    function Func37 return F750A00.Lim_Comp is
    begin
       return F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp);          -- ERROR:
    end Func37;

    function Func38 return F750A00.Lim_Comp is
    begin
       return F750A00.Lim_Comp'(F750A00.Var_Lim_Comp);           -- ERROR:
    end Func38;

    function Func39 return F750A00.Lim_Comp is
    begin
       return F750A00.Lim_Comp(F750A00.Func_Lim_Comp);           -- ERROR:
    end Func39;

    function Func41 return F750A00.Lim_Ext is
    begin
       return F750A00.Func_Lim_Ext;                              -- OK.
    end Func41;

    function Func42 return F750A00.Lim_Ext is
    begin
       return F750A00.Cnst_Lim_Ext;                              -- ERROR:
    end Func42;

    function Func43 return F750A00.Lim_Ext is
    begin
       return F750A00.Var_Lim_Ext;                               -- ERROR:
    end Func43;

    function Func44 return F750A00.Lim_Rec is
    begin
       return F750A00.Func_Lim_Ext.R;                            -- ERROR:
    end Func44;

    function Func45 return F750A00.Lim_Rec is
    begin
       return (F750A00.Func_Lim_Ext.R);                          -- ERROR:
    end Func45;

    function Func46 return F750A00.Lim_Rec is
    begin
       return F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R);          -- ERROR:
    end Func46;

    function Func47 return F750A00.Lim_Rec is
    begin
       return Obj41.R;                                           -- ERROR:
    end Func47;

    function Func48 return F750A00.Lim_Rec is
    begin
       return (Obj41.R);                                         -- ERROR:
    end Func48;

    function Func49 return F750A00.Lim_Rec is
    begin
       return F750A00.Lim_Rec'(Obj41.R);                         -- ERROR:
    end Func49;

    function Func50 return F750A00.Lim_Ext is
    begin
       return Ren51;                                             -- ERROR:
    end Func50;

    function Func51 return F750A00.Lim_Ext is
    begin
       return (Ren51);                                           -- ERROR:
    end Func51;

    function Func52 return F750A00.Lim_Ext is
    begin
       return F750A00.Lim_Ext'(Ren51);                           -- ERROR:
    end Func52;

    function Func53 return F750A00.Lim_Tagged'Class is
    begin
       return F750A00.Func_Lim_Tagged (True);                    -- OK.
    end Func53;

    function Func54 return F750A00.Lim_Tagged'Class is
    begin
       return ((F750A00.Func_Lim_Tagged (False)));               -- OK.
    end Func54;

    function Func55 return F750A00.Lim_Tagged'Class is
    begin
       return F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False)));           -- OK.
    end Func55;

    function Func56 return F750A00.Lim_Tagged'Class is
    begin
       return F750A00.Func_Lim_Tagged_Access (False).all;        -- ERROR:
    end Func56;

    function Func57 return F750A00.Lim_Tagged'Class is
    begin
       return (F750A00.Func_Lim_Tagged_Access (True).all);       -- ERROR:
    end Func57;

    function Func58 return F750A00.Lim_Tagged'Class is
    begin
       return F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all);  -- ERROR:
    end Func58;

    function Func59 return F750A00.Lim_Tagged'Class is
    begin
       return F750A00.Obj_Any_Tagged_Access.all;                 -- ERROR:
    end Func59;

    function Func60 return F750A00.Lim_Tagged'Class is
    begin
       return (F750A00.Obj_Any_Tagged_Access.all);               -- ERROR:
    end Func60;

    function Func61 return F750A00.Lim_Tagged'Class is
    begin
       return F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all);          -- ERROR:
    end Func61;

    function Func62 return F750A00.Lim_Rec is
    begin
       return F750A00.Func_Lim_Tagged_Access (False).R;          -- ERROR:
    end Func62;

    function Func63 return F750A00.Lim_Rec is
    begin
       return F750A00.Obj_Any_Tagged_Access.R;                   -- ERROR:
    end Func63;

begin
    null;
end B750A06;
