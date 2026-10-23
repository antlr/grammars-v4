-- B750A13.A
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
--     In the return expression of an expression function,
--     an expression of a limited type cannot be a conditional expression
--     which has a dependent expression that is anything other than an
--     aggregate, function call, a qualified or parenthesized expression whose
--     operand would be allowed, or a conditional expression all of whose
--     dependent expressions would be allowed.
--
-- TEST DESCRIPTION:
--     We try every kind of name of an entity with a limited type as
--     dependent expression of both if and case expressions:
--     constant and variable names (including those that are dereferenced,
--     selected, indexed, and sliced), function calls that are dereferenced,
--     selected, indexed, and sliced, object renames of function calls,
--     and type conversions to limited types.
--     We also try various combinations of qualified and parenthisized
--     expressions of the above.
--
--     This is a Ada 2012 companion test to test B750A07, which tries
--     Amendment 1 cases.
--
--     We also try raise expressions; for now those cases are commented out
--     as their status in limited expressions needs to be discussed by the
--     ARG.
--
-- CHANGE HISTORY:
--      03 Sep 15   RLB   Created.
--
--!

with F750A00;
procedure B750A13 is

    B : Boolean := True;
    subtype Tiny is Integer range 1 .. 5;
    V : Tiny := 4;
    TBD_Error : exception;

    Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;        -- OK.
    Ren41 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;        -- OK.

    -- If expressions:
    function Func01 return F750A00.Lim_Rec is
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Func_Lim_Rec);                        -- OK.

    function Func02 return F750A00.Lim_Rec is
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Cnst_Lim_Rec);                        -- ERROR:

    function Func03 return F750A00.Lim_Rec is
        (if B then F750A00.Var_Lim_Rec                           -- ERROR:
              else F750A00.Func_Lim_Rec);

    function Func04 return F750A00.Lim_Rec is
        (if B then F750A00.Func_Lim_Rec
              else (F750A00.Func_Lim_Rec));                      -- OK.

    function Func05 return F750A00.Lim_Rec is
        (if B then F750A00.Func_Lim_Rec
         elsif B then (F750A00.Cnst_Lim_Rec)                     -- ERROR:
         else (12, B));

    function Func06 return F750A00.Lim_Rec is
        (if B then (52, False)
              else (F750A00.Var_Lim_Rec));                       -- ERROR:

    function Func07 return F750A00.Lim_Rec is
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));      -- OK.

    function Func08 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));      -- ERROR:

    function Func09 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));       -- ERROR:

    function Func10 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Lim_Rec(F750A00.Func_Lim_Rec));       -- ERROR:

    function Func11 return F750A00.Lim_Rec is
        (if B then F750A00.Func_Lim_Rec
              else Ren01);                                       -- ERROR:


--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--    function Func12 return F750A00.Lim_Rec is
--       (raise TBD_Error);                                      -- OK.
--
--    function Func13 return F750A00.Lim_Rec is
--        (if B then F750A00.Func_Lim_Rec
--              else raise Program_Error);                         -- OK.
--*** End replace.

    function Func14 return F750A00.Lim_Array is
        (if B then F750A00.Func_Lim_Array
              else F750A00.Func_Lim_Array(1..2));                -- ERROR:

    function Func15 return F750A00.Lim_Rec is
        (if B then (87, True)
              else F750A00.Func_Lim_Array(2));                   -- ERROR:

    function Func16 return F750A00.Lim_Rec is
        (if B then (87, True)
              else (F750A00.Func_Lim_Array(2)));                 -- ERROR:

    function Func17 return F750A00.Lim_Rec is
        (if B then (87, True)
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))); -- ERROR:

    function Func18 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Func_Lim_Tagged_Access (False).R);    -- ERROR:

    function Func19 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Obj_Any_Tagged_Access.R);             -- ERROR:


    function Func20 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else F750A00.Func_Lim_Comp);                       -- OK.

    function Func21 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else F750A00.Cnst_Lim_Comp);                       -- ERROR:

    function Func22 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else F750A00.Var_Lim_Comp);                        -- ERROR:

    function Func23 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else (F750A00.Func_Lim_Comp));                     -- OK.

    function Func24 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else (F750A00.Cnst_Lim_Comp));                     -- ERROR:

    function Func25 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else (F750A00.Var_Lim_Comp));                      -- ERROR:

    function Func26 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));    -- OK.

    function Func27 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));    -- ERROR:

    function Func28 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));     -- ERROR:

    function Func29 return F750A00.Lim_Comp is
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp(F750A00.Func_Lim_Comp));     -- ERROR:


    function Func31 return F750A00.Lim_Ext is
        (if B then F750A00.Func_Lim_Ext                          -- OK.
              else (R => (12, False), G => 87, N => 27));

    function Func32 return F750A00.Lim_Ext is
        (if B then (if B and True then F750A00.Func_Lim_Ext
                    else F750A00.Cnst_Lim_Ext)                   -- ERROR:
              else (R => (12, False), G => 87, N => 27));

    function Func33 return F750A00.Lim_Ext is
        (if B then F750A00.Var_Lim_Ext                           -- ERROR:
              else (R => (12, False), G => 87, N => 27));

    function Func34 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Func_Lim_Ext.R);                      -- ERROR:

    function Func35 return F750A00.Lim_Rec is
        (if B then (52, False)
              else (F750A00.Func_Lim_Ext.R));                    -- ERROR:

    function Func36 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));    -- ERROR:

    function Func37 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Cnst_Lim_Ext.R);                      -- ERROR:

    function Func38 return F750A00.Lim_Rec is
        (if B then (52, False)
              else (F750A00.Cnst_Lim_Ext.R));                    -- ERROR:

    function Func39 return F750A00.Lim_Rec is
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Cnst_Lim_Ext.R));    -- ERROR:

    function Func40 return F750A00.Lim_Ext is
        (if B then (if not B then F750A00.Func_Lim_Ext
                   else Ren41)                                   -- ERROR:
              else (R => (12, False), G => 87, N => 27));

    function Func41 return F750A00.Lim_Ext is
        (if B then (Ren41)                                       -- ERROR:
              else (R => (12, False), G => 87, N => 27));

    function Func42 return F750A00.Lim_Ext is
        (if B then F750A00.Lim_Ext'(Ren41)                       -- ERROR:
              else (R => (12, False), G => 87, N => 27));


    function Func43 return F750A00.Lim_Tagged'Class is
        (if B then F750A00.Func_Lim_Tagged (True)
         elsif not B then F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False)))
              else F750A00.Func_Lim_Tagged (False));             -- OK.

    function Func44 return F750A00.Lim_Tagged'Class is
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Func_Lim_Tagged_Access (False).all);  -- ERROR:

    function Func45 return F750A00.Lim_Tagged'Class is
        (if B then F750A00.Func_Lim_Tagged (True)
              else (F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:

    function Func46 return F750A00.Lim_Tagged'Class is
        (if B then F750A00.Func_Lim_Tagged (True)
              else  F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:

    function Func47 return F750A00.Lim_Tagged'Class is
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Obj_Any_Tagged_Access.all);           -- ERROR:

    function Func48 return F750A00.Lim_Tagged'Class is
        (if B then F750A00.Func_Lim_Tagged (True)
              else (F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:

    function Func49 return F750A00.Lim_Tagged'Class is
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:


    -- Case expressions:
    function Func51 return F750A00.Lim_Rec is
        (case V is
           when 1 .. 3 => F750A00.Func_Lim_Rec,
           when 4 .. 5 => F750A00.Func_Lim_Rec);                 -- OK.

    function Func52 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Rec,
            when others => F750A00.Cnst_Lim_Rec);                -- ERROR:

    function Func53 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 2 => F750A00.Var_Lim_Rec,                  -- ERROR:
            when 3 .. 5 => F750A00.Func_Lim_Rec);

    function Func54 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Rec,
            when 4 .. 5 => (F750A00.Func_Lim_Rec));              -- OK.

    function Func55 return F750A00.Lim_Rec is
        (case V is
           when 1 => F750A00.Func_Lim_Rec,
           when 2 => (F750A00.Cnst_Lim_Rec),                     -- ERROR:
           when others => (12, B));

    function Func56 return F750A00.Lim_Rec is
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => (F750A00.Var_Lim_Rec));                -- ERROR:

    function Func57 return F750A00.Lim_Rec is
        (case V is
           when 1 .. 4 => F750A00.Func_Lim_Rec,
           when 5 => F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));    -- OK.

    function Func58 return F750A00.Lim_Rec is
        (case V is
           when 1 .. 3 | 5 => (52, False),
           when 4 => F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));    -- ERROR:

    function Func59 return F750A00.Lim_Rec is
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));-- ERROR:

    function Func60 return F750A00.Lim_Rec is
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => F750A00.Lim_Rec(F750A00.Func_Lim_Rec));-- ERROR:

    function Func61 return F750A00.Lim_Rec is
        (case V is
           when 1 .. 3 => F750A00.Func_Lim_Rec,
           when 4 .. 5 => Ren01);                                -- ERROR:


--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--    function Func62 return F750A00.Lim_Rec is
--        (case V is
--           when 1 .. 3 => F750A00.Func_Lim_Rec,
--           when 4 .. 5 => raise Program_Error);                  -- OK.
--
--    function Func63x return F750A00.Lim_Rec is
--        (case V is
--           when 1 .. 3 => F750A00.Func_Lim_Rec,
--           when others => raise Program_Error);                  -- OK.
--*** End replace.

    function Func64 return F750A00.Lim_Array is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Array,
            when 4 .. 5 => F750A00.Func_Lim_Array(1..2));        -- ERROR:

    function Func65 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (87, True),
            when 4 .. 5 => F750A00.Func_Lim_Array(2));           -- ERROR:

    function Func66 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (87, True),
            when others => (F750A00.Func_Lim_Array(2)));         -- ERROR:

    function Func67 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (87, True),
            when 4 .. 5 =>
                   F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))); -- ERROR:

    function Func68 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (52, False),
            when others =>
                   F750A00.Func_Lim_Tagged_Access (False).R);    -- ERROR:

    function Func69 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Obj_Any_Tagged_Access.R);     -- ERROR:


    function Func70 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => F750A00.Func_Lim_Comp);               -- OK.

    function Func71 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others => F750A00.Cnst_Lim_Comp);               -- ERROR:

    function Func72 return F750A00.Lim_Comp is
        (case V is
            when 2 | 4 => (P => <>, N => 12),
            when others => F750A00.Var_Lim_Comp);                -- ERROR:

    function Func73 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Func_Lim_Comp));             -- OK.

    function Func74 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Cnst_Lim_Comp));             -- ERROR:

    function Func75 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Var_Lim_Comp));              -- ERROR:

    function Func76 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others =>
                   F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));    -- OK.

    function Func77 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 =>
                   F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));    -- ERROR:

    function Func78 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 =>
                   F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));     -- ERROR:

    function Func79 return F750A00.Lim_Comp is
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others =>
                   F750A00.Lim_Comp(F750A00.Func_Lim_Comp));     -- ERROR:


    function Func81 return F750A00.Lim_Ext is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Ext,                 -- OK.
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));

    function Func82 return F750A00.Lim_Ext is
        (case V is
            when 1 .. 3 => (if B and True then F750A00.Func_Lim_Ext
                            else F750A00.Cnst_Lim_Ext),          -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));

    function Func83 return F750A00.Lim_Ext is
        (if B then
           (case V is
              when 1 .. 3 => F750A00.Var_Lim_Ext,                -- ERROR:
              when others => (R => (12, False), G => 87, N => 27))
         else F750A00.Func_Lim_Ext);

    function Func84 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Func_Lim_Ext.R);              -- ERROR:

    function Func85 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => (F750A00.Func_Lim_Ext.R));            -- ERROR:

    function Func86 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (52, False),
            when others =>
                   F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));    -- ERROR:

    function Func87 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Cnst_Lim_Ext.R);              -- ERROR:

    function Func88 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => (F750A00.Cnst_Lim_Ext.R));            -- ERROR:

    function Func89 return F750A00.Lim_Rec is
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 =>
                   F750A00.Lim_Rec'(F750A00.Cnst_Lim_Ext.R));    -- ERROR:

    function Func90 return F750A00.Lim_Ext is
        (case V is
            when 1 .. 3 => (if not B then F750A00.Func_Lim_Ext
               else Ren41),                                      -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));

    function Func91 return F750A00.Lim_Ext is
        (if B then
           (case V is
              when 1 | 3 => (Ren41),                             -- ERROR:
              when 2 | 4 .. 5 => (R => (12, False), G => 87, N => 27))
         else F750A00.Func_Lim_Ext);

    function Func92 return F750A00.Lim_Ext is
        (case V is
            when 1 .. 3 => F750A00.Lim_Ext'(Ren41),              -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));


    function Func93 return F750A00.Lim_Tagged'Class is
        (case V is
            when 1 => F750A00.Func_Lim_Tagged (True),
            when 2 => F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False))),
            when 3 .. 5 => F750A00.Func_Lim_Tagged (False));     -- OK.

    function Func94 return F750A00.Lim_Tagged'Class is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 =>
                   F750A00.Func_Lim_Tagged_Access (False).all);  -- ERROR:

    function Func95 return F750A00.Lim_Tagged'Class is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when others =>
                   (F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:

    function Func96 return F750A00.Lim_Tagged'Class is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 =>  F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:

    function Func97 return F750A00.Lim_Tagged'Class is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 => F750A00.Obj_Any_Tagged_Access.all);   -- ERROR:

    function Func98 return F750A00.Lim_Tagged'Class is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 => (F750A00.Obj_Any_Tagged_Access.all)); -- ERROR:

    function Func99 return F750A00.Lim_Tagged'Class is
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when others => F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:

begin
    null;
end B750A13;
