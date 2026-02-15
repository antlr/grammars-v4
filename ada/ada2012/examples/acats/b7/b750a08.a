-- B750A08.A
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
--     of a limited type cannot be a conditional expression which has a
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
--     We also try various combinations of qualified and parenthisized
--     expressions of the above.
--
--     This is a Ada 2012 companion test to test B750A01, which tries
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
procedure B750A08 is

    B : Boolean := True;
    subtype Tiny is Integer range 1 .. 5;
    V : Tiny := 4;
    TBD_Error : exception;

    Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;        -- OK.
    Ren41 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;        -- OK.

    -- If expressions:
    Obj01 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Func_Lim_Rec);                        -- OK.
    Obj02 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Cnst_Lim_Rec);                        -- ERROR:
    Obj03 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Var_Lim_Rec                           -- ERROR:
              else F750A00.Func_Lim_Rec);
    Obj04 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else (F750A00.Func_Lim_Rec));                      -- OK.
    Obj05 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
         elsif B then (F750A00.Cnst_Lim_Rec)                     -- ERROR:
         else (12, B));
    Obj06 : constant F750A00.Lim_Rec :=
        (if B then (52, False)
              else (F750A00.Var_Lim_Rec));                       -- ERROR:
    Obj07 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));      -- OK.
    Obj08 : constant F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));      -- ERROR:
    Obj09 : constant F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));       -- ERROR:
    Obj10 : constant F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec(F750A00.Func_Lim_Rec));       -- ERROR:
    Obj11 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else Ren01);                                       -- ERROR:

--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--    Obj12 : constant F750A00.Lim_Rec := (raise TBD_Error);       -- OK.
--    Obj13 : constant F750A00.Lim_Rec :=
--        (if B then F750A00.Func_Lim_Rec
--              else raise Program_Error);                         -- OK.
--*** End replace.

    Obj14 : constant F750A00.Lim_Array :=
        (if B then F750A00.Func_Lim_Array
              else F750A00.Func_Lim_Array(1..2));                -- ERROR:
    Obj15 : constant F750A00.Lim_Rec :=
        (if B then (87, True)
              else F750A00.Func_Lim_Array(2));                   -- ERROR:
    Obj16 : constant F750A00.Lim_Rec :=
        (if B then (87, True)
              else (F750A00.Func_Lim_Array(2)));                 -- ERROR:
    Obj17 : constant F750A00.Lim_Rec :=
        (if B then (87, True)
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))); -- ERROR:
    Obj18 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Func_Lim_Tagged_Access (False).R);    -- ERROR:
    Obj19 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Obj_Any_Tagged_Access.R);             -- ERROR:

    Obj20 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Func_Lim_Comp);                       -- OK.
    Obj21 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Cnst_Lim_Comp);                       -- ERROR:
    Obj22 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Var_Lim_Comp);                        -- ERROR:
    Obj23 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else (F750A00.Func_Lim_Comp));                     -- OK.
    Obj24 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else (F750A00.Cnst_Lim_Comp));                     -- ERROR:
    Obj25 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else (F750A00.Var_Lim_Comp));                      -- ERROR:
    Obj26 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));    -- OK.
    Obj27 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));    -- ERROR:
    Obj28 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));     -- ERROR:
    Obj29 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp(F750A00.Func_Lim_Comp));     -- ERROR:

    Obj31 : F750A00.Lim_Ext :=
        (if B then F750A00.Func_Lim_Ext                          -- OK.
              else (R => (12, False), G => 87, N => 27));
    Obj32 : F750A00.Lim_Ext :=
        (if B then (if B and True then F750A00.Func_Lim_Ext
                    else F750A00.Cnst_Lim_Ext)                   -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    Obj33 : F750A00.Lim_Ext :=
        (if B then F750A00.Var_Lim_Ext                           -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    Obj34 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Func_Lim_Ext.R);                      -- ERROR:
    Obj35 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else (F750A00.Func_Lim_Ext.R));                    -- ERROR:
    Obj36 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));    -- ERROR:
    Obj37 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Cnst_Lim_Ext.R);                      -- ERROR:
    Obj38 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else (F750A00.Cnst_Lim_Ext.R));                    -- ERROR:
    Obj39 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Cnst_Lim_Ext.R));    -- ERROR:
    Obj40 : F750A00.Lim_Ext :=
        (if B then (if not B then F750A00.Func_Lim_Ext
                   else Ren41)                                   -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    Obj41 : F750A00.Lim_Ext :=
        (if B then (Ren41)                                       -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    Obj42 : F750A00.Lim_Ext :=
        (if B then F750A00.Lim_Ext'(Ren41)                       -- ERROR:
              else (R => (12, False), G => 87, N => 27));

    Obj43 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
         elsif not B then F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False)))
              else F750A00.Func_Lim_Tagged (False));             -- OK.
    Obj44 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Func_Lim_Tagged_Access (False).all);  -- ERROR:
    Obj45 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else (F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:
    Obj46 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else  F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:
    Obj47 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Obj_Any_Tagged_Access.all);           -- ERROR:
    Obj48 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else (F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:
    Obj49 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:

    -- Case expressions:
    Obj51 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => F750A00.Func_Lim_Rec,
           when 4 .. 5 => F750A00.Func_Lim_Rec);                 -- OK.
    Obj52 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Rec,
            when others => F750A00.Cnst_Lim_Rec);                -- ERROR:
    Obj53 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 2 => F750A00.Var_Lim_Rec,                  -- ERROR:
            when 3 .. 5 => F750A00.Func_Lim_Rec);
    Obj54 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Rec,
            when 4 .. 5 => (F750A00.Func_Lim_Rec));              -- OK.
    Obj55 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 => F750A00.Func_Lim_Rec,
           when 2 => (F750A00.Cnst_Lim_Rec),                     -- ERROR:
           when others => (12, B));
    Obj56 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => (F750A00.Var_Lim_Rec));                -- ERROR:
    Obj57 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 4 => F750A00.Func_Lim_Rec,
           when 5 => F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));    -- OK.
    Obj58 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 | 5 => (52, False),
           when 4 => F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));    -- ERROR:
    Obj59 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));-- ERROR:
    Obj60 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => F750A00.Lim_Rec(F750A00.Func_Lim_Rec));-- ERROR:
    Obj61 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => F750A00.Func_Lim_Rec,
           when 4 .. 5 => Ren01);                                -- ERROR:

--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--    Obj62 : constant F750A00.Lim_Rec :=
--        (case V is
--           when 1 .. 3 => F750A00.Func_Lim_Rec,
--           when 4 .. 5 => raise Program_Error);                  -- OK.
--    Obj63 : constant F750A00.Lim_Rec :=
--        (case V is
--           when 1 .. 3 => F750A00.Func_Lim_Rec,
--           when others => raise Program_Error);                  -- OK.
--*** End replace.

    Obj64 : constant F750A00.Lim_Array :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Array,
            when 4 .. 5 => F750A00.Func_Lim_Array(1..2));        -- ERROR:
    Obj65 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (87, True),
            when 4 .. 5 => F750A00.Func_Lim_Array(2));           -- ERROR:
    Obj66 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (87, True),
            when others => (F750A00.Func_Lim_Array(2)));         -- ERROR:
    Obj67 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (87, True),
            when 4 .. 5 =>
                   F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))); -- ERROR:
    Obj68 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when others =>
                   F750A00.Func_Lim_Tagged_Access (False).R);    -- ERROR:
    Obj69 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Obj_Any_Tagged_Access.R);     -- ERROR:

    Obj70 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => F750A00.Func_Lim_Comp);               -- OK.
    Obj71 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others => F750A00.Cnst_Lim_Comp);               -- ERROR:
    Obj72 : F750A00.Lim_Comp :=
        (case V is
            when 2 | 4 => (P => <>, N => 12),
            when others => F750A00.Var_Lim_Comp);                -- ERROR:
    Obj73 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Func_Lim_Comp));             -- OK.
    Obj74 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Cnst_Lim_Comp));             -- ERROR:
    Obj75 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Var_Lim_Comp));              -- ERROR:
    Obj76 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others =>
                   F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));    -- OK.
    Obj77 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 =>
                   F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));    -- ERROR:
    Obj78 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 =>
                   F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));     -- ERROR:
    Obj79 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others =>
                   F750A00.Lim_Comp(F750A00.Func_Lim_Comp));     -- ERROR:

    Obj81 : F750A00.Lim_Ext :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Ext,                 -- OK.
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    Obj82 : F750A00.Lim_Ext :=
        (case V is
            when 1 .. 3 => (if B and True then F750A00.Func_Lim_Ext
                            else F750A00.Cnst_Lim_Ext),          -- ERROR:
              when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    Obj83 : F750A00.Lim_Ext :=
        (if B then
           (case V is
              when 1 .. 3 => F750A00.Var_Lim_Ext,                -- ERROR:
              when others => (R => (12, False), G => 87, N => 27))
         else F750A00.Func_Lim_Ext);
    Obj84 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Func_Lim_Ext.R);              -- ERROR:
    Obj85 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => (F750A00.Func_Lim_Ext.R));            -- ERROR:
    Obj86 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when others =>
                   F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));    -- ERROR:
    Obj87 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Cnst_Lim_Ext.R);              -- ERROR:
    Obj88 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => (F750A00.Cnst_Lim_Ext.R));            -- ERROR:
    Obj89 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 =>
                   F750A00.Lim_Rec'(F750A00.Cnst_Lim_Ext.R));    -- ERROR:
    Obj90 : F750A00.Lim_Ext :=
        (case V is
            when 1 .. 3 => (if not B then F750A00.Func_Lim_Ext
               else Ren41),                                      -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    Obj91 : F750A00.Lim_Ext :=
        (if B then
           (case V is
              when 1 | 3 => (Ren41),                             -- ERROR:
              when 2 | 4 .. 5 => (R => (12, False), G => 87, N => 27))
         else F750A00.Func_Lim_Ext);
    Obj92 : F750A00.Lim_Ext :=
        (case V is
            when 1 .. 3 => F750A00.Lim_Ext'(Ren41),              -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));

    Obj93 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 => F750A00.Func_Lim_Tagged (True),
            when 2 => F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False))),
            when 3 .. 5 => F750A00.Func_Lim_Tagged (False));     -- OK.
    Obj94 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 =>
                   F750A00.Func_Lim_Tagged_Access (False).all);  -- ERROR:
    Obj95 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when others =>
                   (F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:
    Obj96 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 =>  F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:
    Obj97 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 => F750A00.Obj_Any_Tagged_Access.all);   -- ERROR:
    Obj98 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 => (F750A00.Obj_Any_Tagged_Access.all)); -- ERROR:
    Obj99 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when others => F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:

begin
    null;
end B750A08;
