-- B750A10.A
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
--     In the qualified expression of an allocator, an expression of a
--     limited type cannot be a conditional expression which has a
--     dependent expression that is anything other than an aggregate, function
--     call, a qualified or parenthesized expression whose operand would
--     be allowed, or a conditional expression all of whose dependent
--     expressions would be allowed.
--
-- TEST DESCRIPTION:
--     We try every kind of name of an entity with a limited type as
--     dependent expression of both if and case expressions:
--     constant and variable names (including those that are dereferenced,
--     selected, indexed, and sliced), function calls that are dereferenced,
--     selected, indexed, and sliced, object renames of function calls,
--     and type conversions to limited types.
--     We also try various combinations of parenthisized expressions of the
--     above. (We don't specifically try qualified expressions in this test,
--     since essentially the entire test is trying that; it is always present
--     in the syntax of the allocator.)
--
--     This is a Ada 2012 companion test to test B750A04, which tries
--     Amendment 1 cases.
--
--     We also try raise expressions; for now those cases are commented out
--     as their status in limited expressions needs to be discussed by the
--     ARG.
--
-- CHANGE HISTORY:
--      02 Sep 15   RLB   Created.
--
--!

with F750A00;
procedure B750A10 is

    B : Boolean := True;
    subtype Tiny is Integer range 1 .. 5;
    V : Tiny := 4;
    TBD_Error : exception;

    Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;        -- OK.
    Ren41 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;        -- OK.

    type Acc_Lim_Rec is access all F750A00.Lim_Rec;
    type Acc_Lim_Comp is access all F750A00.Lim_Comp;

    -- If expressions:
    Obj01 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Func_Lim_Rec));                       -- OK.
    Obj02 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Cnst_Lim_Rec));                       -- ERROR:
    Obj03 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then F750A00.Var_Lim_Rec                           -- ERROR:
              else F750A00.Func_Lim_Rec));
    Obj04 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then F750A00.Func_Lim_Rec
              else (F750A00.Func_Lim_Rec)));                     -- OK.
    Obj05 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then F750A00.Func_Lim_Rec
         elsif B then (F750A00.Cnst_Lim_Rec)                     -- ERROR:
         else (12, B)));
    Obj06 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (52, False)
              else (F750A00.Var_Lim_Rec)));                      -- ERROR:
    Obj10 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (52, False)
              else F750A00.Lim_Rec(F750A00.Func_Lim_Rec)));      -- ERROR:
    Obj11 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then F750A00.Func_Lim_Rec
              else Ren01));                                      -- ERROR:

--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--    Obj12 : Acc_Lim_Rec :=
--                 new F750A00.Lim_Rec'((raise TBD_Error));      -- OK.
--    Obj13 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
--        (if B then F750A00.Func_Lim_Rec
--              else raise Program_Error));                      - OK.
--*** End replace.

    Obj14 : access F750A00.Lim_Array := new F750A00.Lim_Array'(
        (if B then F750A00.Func_Lim_Array
              else F750A00.Func_Lim_Array(1..2)));               -- ERROR:
    Obj15 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (87, True)
              else F750A00.Func_Lim_Array(2)));                  -- ERROR:
    Obj16 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (87, True)
              else (F750A00.Func_Lim_Array(2))));                -- ERROR:
    Obj17 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (87, True)
              else
                 F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))));  -- ERROR:
    Obj18 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (52, False)
              else F750A00.Func_Lim_Tagged_Access (False).R));   -- ERROR:
    Obj19 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (52, False)
              else F750A00.Obj_Any_Tagged_Access.R));            -- ERROR:

    Obj20 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (if B then (P => <>, N => 12)
              else F750A00.Func_Lim_Comp));                      -- OK.
    Obj21 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (if B then (P => <>, N => 12)
              else F750A00.Cnst_Lim_Comp));                      -- ERROR:
    Obj22 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (if B then (P => <>, N => 12)
              else F750A00.Var_Lim_Comp));                       -- ERROR:
    Obj23 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (if B then (P => <>, N => 12)
              else (F750A00.Func_Lim_Comp)));                    -- OK.
    Obj24 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (if B then (P => <>, N => 12)
              else (F750A00.Cnst_Lim_Comp)));                    -- ERROR:
    Obj25 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (if B then (P => <>, N => 12)
              else (F750A00.Var_Lim_Comp)));                     -- ERROR:
    Obj29 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (if B then (P => <>, N => 12)
              else Acc_Lim_Comp(F750A00.Func_Lim_Comp)));        -- ERROR:

    Obj31 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
        (if B then F750A00.Func_Lim_Ext                          -- OK.
              else (R => (12, False), G => 87, N => 27)));
    Obj32 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
        (if B then (if B and True then F750A00.Func_Lim_Ext
                    else F750A00.Cnst_Lim_Ext)                   -- ERROR:
              else (R => (12, False), G => 87, N => 27)));
    Obj33 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
        (if B then F750A00.Var_Lim_Ext                           -- ERROR:
              else (R => (12, False), G => 87, N => 27)));
    Obj34 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (52, False)
              else F750A00.Func_Lim_Ext.R));                     -- ERROR:
    Obj35 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (52, False)
              else (F750A00.Func_Lim_Ext.R)));                   -- ERROR:
    Obj37 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (52, False)
              else F750A00.Cnst_Lim_Ext.R));                     -- ERROR:
    Obj38 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (if B then (52, False)
              else (F750A00.Cnst_Lim_Ext.R)));                   -- ERROR:
    Obj40 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
        (if B then (if not B then F750A00.Func_Lim_Ext
                   else Ren41)                                   -- ERROR:
              else (R => (12, False), G => 87, N => 27)));
    Obj41 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
        (if B then (Ren41)                                       -- ERROR:
              else (R => (12, False), G => 87, N => 27)));
    Obj42 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(
        (if B then F750A00.Lim_Ext'(Ren41)                       -- ERROR:
              else (R => (12, False), G => 87, N => 27)));

    Obj43 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (if B then F750A00.Func_Lim_Tagged (True)
         elsif not B then F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False)))
              else F750A00.Func_Lim_Tagged (False)));            -- OK.
    Obj44 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Func_Lim_Tagged_Access (False).all)); -- ERROR:
    Obj45 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (if B then F750A00.Func_Lim_Tagged (True)
              else (F750A00.Func_Lim_Tagged_Access (True).all)));-- ERROR:
    Obj46 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (if B then F750A00.Func_Lim_Tagged (True)
              else  F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all)));-- ERROR:
    Obj47 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Obj_Any_Tagged_Access.all));          -- ERROR:
    Obj48 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (if B then F750A00.Func_Lim_Tagged (True)
              else (F750A00.Obj_Any_Tagged_Access.all)));        -- ERROR:
    Obj49 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all)));        -- ERROR:

    -- Case expressions:
    Obj51 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
           when 1 .. 3 => F750A00.Func_Lim_Rec,
           when 4 .. 5 => F750A00.Func_Lim_Rec));                -- OK.
    Obj52 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Rec,
            when others => F750A00.Cnst_Lim_Rec));               -- ERROR:
    Obj53 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 2 => F750A00.Var_Lim_Rec,                  -- ERROR:
            when 3 .. 5 => F750A00.Func_Lim_Rec));
    Obj54 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Rec,
            when 4 .. 5 => (F750A00.Func_Lim_Rec)));             -- OK.
    Obj55 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
           when 1 => F750A00.Func_Lim_Rec,
           when 2 => (F750A00.Cnst_Lim_Rec),                     -- ERROR:
           when others => (12, B)));
    Obj56 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => (F750A00.Var_Lim_Rec)));               -- ERROR:
    Obj60 : Acc_Lim_Rec := new F750A00.Lim_Rec'(case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => F750A00.Lim_Rec(F750A00.Func_Lim_Rec));-- ERROR:
    Obj61 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
           when 1 .. 3 => F750A00.Func_Lim_Rec,
           when 4 .. 5 => Ren01));                               -- ERROR:

--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--    Obj62 : Acc_Lim_Rec := new F750A00.Lim_Rec'(case V is
--           when 1 .. 3 => F750A00.Func_Lim_Rec,
--           when 4 .. 5 => raise Program_Error);                  -- OK.
--    Obj63 : Acc_Lim_Rec := new F750A00.Lim_Rec'(case V is
--           when 1 .. 3 => F750A00.Func_Lim_Rec,
--           when others => raise Program_Error);                  -- OK.
--*** End replace.

    Obj64 : access F750A00.Lim_Array := new F750A00.Lim_Array'(case V is
            when 1 .. 3 => F750A00.Func_Lim_Array,
            when 4 .. 5 => F750A00.Func_Lim_Array(1..2));        -- ERROR:
    Obj65 : Acc_Lim_Rec := new F750A00.Lim_Rec'(case V is
            when 1 .. 3 => (87, True),
            when 4 .. 5 => F750A00.Func_Lim_Array(2));           -- ERROR:
    Obj66 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => (87, True),
            when others => (F750A00.Func_Lim_Array(2))));        -- ERROR:
    Obj68 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => (52, False),
            when others =>
                   F750A00.Func_Lim_Tagged_Access (False).R));   -- ERROR:
    Obj69 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Obj_Any_Tagged_Access.R));    -- ERROR:

    Obj70 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => F750A00.Func_Lim_Comp));              -- OK.
    Obj71 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others => F750A00.Cnst_Lim_Comp));              -- ERROR:
    Obj72 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (case V is
            when 2 | 4 => (P => <>, N => 12),
            when others => F750A00.Var_Lim_Comp));               -- ERROR:
    Obj73 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Func_Lim_Comp)));            -- OK.
    Obj74 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Cnst_Lim_Comp)));            -- ERROR:
    Obj75 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Var_Lim_Comp)));             -- ERROR:
    Obj79 : Acc_Lim_Comp := new F750A00.Lim_Comp'(
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others =>
                   F750A00.Lim_Comp(F750A00.Func_Lim_Comp)));    -- ERROR:

    Obj81 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(case V is
            when 1 .. 3 => F750A00.Func_Lim_Ext,                 -- OK.
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    Obj82 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(case V is
            when 1 .. 3 => (if B and True then F750A00.Func_Lim_Ext
                            else F750A00.Cnst_Lim_Ext),          -- ERROR:
              when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    Obj83 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(if B then
           (case V is
              when 1 .. 3 => F750A00.Var_Lim_Ext,                -- ERROR:
              when others => (R => (12, False), G => 87, N => 27))
         else F750A00.Func_Lim_Ext);
    Obj84 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Func_Lim_Ext.R));             -- ERROR:
    Obj85 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => (F750A00.Func_Lim_Ext.R)));           -- ERROR:
    Obj87 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Cnst_Lim_Ext.R));             -- ERROR:
    Obj88 : Acc_Lim_Rec := new F750A00.Lim_Rec'(
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => (F750A00.Cnst_Lim_Ext.R)));           -- ERROR:
    Obj90 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(case V is
            when 1 .. 3 => (if not B then F750A00.Func_Lim_Ext
               else Ren41),                                      -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    Obj91 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(if B then
           (case V is
              when 1 | 3 => (Ren41),                             -- ERROR:
              when 2 | 4 .. 5 => (R => (12, False), G => 87, N => 27))
         else F750A00.Func_Lim_Ext);
    Obj92 : access F750A00.Lim_Ext := new F750A00.Lim_Ext'(case V is
            when 1 .. 3 => F750A00.Lim_Ext'(Ren41),              -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));

    Obj93 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (case V is
            when 1 => F750A00.Func_Lim_Tagged (True),
            when 2 => F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False))),
            when 3 .. 5 => F750A00.Func_Lim_Tagged (False)));    -- OK.
    Obj94 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 =>
                   F750A00.Func_Lim_Tagged_Access (False).all)); -- ERROR:
    Obj95 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when others =>
                   (F750A00.Func_Lim_Tagged_Access (True).all)));-- ERROR:
    Obj96 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 =>  F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all)));-- ERROR:
    Obj97 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 => F750A00.Obj_Any_Tagged_Access.all));  -- ERROR:
    Obj98 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 => (F750A00.Obj_Any_Tagged_Access.all)));-- ERROR:
    Obj99 : F750A00.Any_Tagged_Access := new F750A00.Lim_Tagged'Class'(
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when others => F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all)));        -- ERROR:

begin
    null;
end B750A10;
