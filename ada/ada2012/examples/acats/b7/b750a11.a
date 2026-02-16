-- B750A11.A
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
--     In the initialization expression of an extended return statement,
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
--     This is a Ada 2012 companion test to test B750A05, which tries
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
procedure B750A11 is

    B : Boolean := True;
    subtype Tiny is Integer range 1 .. 5;
    V : Tiny := 4;
    TBD_Error : exception;

    Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;        -- OK.
    Ren41 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;        -- OK.

    -- If expressions:
    function Func01 return F750A00.Lim_Rec is
    begin
       return Ret01 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Func_Lim_Rec);                        -- OK.
    end Func01;

    function Func02 return F750A00.Lim_Rec is
    begin
       return Ret02 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Cnst_Lim_Rec);                        -- ERROR:
    end Func02;

    function Func03 return F750A00.Lim_Rec is
    begin
       return Ret03 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Var_Lim_Rec                           -- ERROR:
              else F750A00.Func_Lim_Rec);
    end Func03;

    function Func04 return F750A00.Lim_Rec is
    begin
       return Ret04 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else (F750A00.Func_Lim_Rec));                      -- OK.
    end Func04;

    function Func05 return F750A00.Lim_Rec is
    begin
       return Ret05 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
         elsif B then (F750A00.Cnst_Lim_Rec)                     -- ERROR:
         else (12, B));
    end Func05;

    function Func06 return F750A00.Lim_Rec is
    begin
       return Ret06 : constant F750A00.Lim_Rec :=
        (if B then (52, False)
              else (F750A00.Var_Lim_Rec));                       -- ERROR:
    end Func06;

    function Func07 return F750A00.Lim_Rec is
    begin
       return Ret07 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));      -- OK.
    end Func07;

    function Func08 return F750A00.Lim_Rec is
    begin
       return Ret08 : constant F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));      -- ERROR:
    end Func08;

    function Func09 return F750A00.Lim_Rec is
    begin
       return Ret09 : constant F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));       -- ERROR:
    end Func09;

    function Func10 return F750A00.Lim_Rec is
    begin
       return Ret10 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec(F750A00.Func_Lim_Rec)) do     -- ERROR:
          Ret10.B := True;
       end return;
    end Func10;

    function Func11 return F750A00.Lim_Rec is
    begin
       return Ret11 : constant F750A00.Lim_Rec :=
        (if B then F750A00.Func_Lim_Rec
              else Ren01);                                       -- ERROR:
    end Func11;


--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--    function Func12 return F750A00.Lim_Rec is
--    begin
--       return Ret12 : constant F750A00.Lim_Rec :=
--                                        (raise TBD_Error);       -- OK.
--    end Func12;
--
--    function Func13 return F750A00.Lim_Rec is
--    begin
--       return Ret13 : constant F750A00.Lim_Rec :=
--        (if B then F750A00.Func_Lim_Rec
--              else raise Program_Error);                         -- OK.
--    end Func13;
--*** End replace.

    function Func14 return F750A00.Lim_Array is
    begin
       return Ret14 : constant F750A00.Lim_Array :=
        (if B then F750A00.Func_Lim_Array
              else F750A00.Func_Lim_Array(1..2));                -- ERROR:
    end Func14;

    function Func15 return F750A00.Lim_Rec is
    begin
       return Ret15 : constant F750A00.Lim_Rec :=
        (if B then (87, True)
              else F750A00.Func_Lim_Array(2));                   -- ERROR:
    end Func15;

    function Func16 return F750A00.Lim_Rec is
    begin
       return Ret16 : constant F750A00.Lim_Rec :=
        (if B then (87, True)
              else (F750A00.Func_Lim_Array(2)));                 -- ERROR:
    end Func16;

    function Func17 return F750A00.Lim_Rec is
    begin
       return Ret17 : constant F750A00.Lim_Rec :=
        (if B then (87, True)
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))); -- ERROR:
    end Func17;

    function Func18 return F750A00.Lim_Rec is
    begin
       return Ret18 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Func_Lim_Tagged_Access (False).R);    -- ERROR:
    end Func18;

    function Func19 return F750A00.Lim_Rec is
    begin
       return Ret19 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Obj_Any_Tagged_Access.R) do            -- ERROR:
          Ret19.B := True;
       end return;
    end Func19;


    function Func20 return F750A00.Lim_Comp is
    begin
       return Ret20 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Func_Lim_Comp);                       -- OK.
    end Func20;

    function Func21 return F750A00.Lim_Comp is
    begin
       return Ret21 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Cnst_Lim_Comp);                       -- ERROR:
    end Func21;

    function Func22 return F750A00.Lim_Comp is
    begin
       return Ret22 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Var_Lim_Comp);                        -- ERROR:
    end Func22;

    function Func23 return F750A00.Lim_Comp is
    begin
       return Ret23 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else (F750A00.Func_Lim_Comp));                     -- OK.
    end Func23;

    function Func24 return F750A00.Lim_Comp is
    begin
       return Ret24 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else (F750A00.Cnst_Lim_Comp));                     -- ERROR:
    end Func24;

    function Func25 return F750A00.Lim_Comp is
    begin
       return Ret25 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else (F750A00.Var_Lim_Comp));                      -- ERROR:
    end Func25;

    function Func26 return F750A00.Lim_Comp is
    begin
       return Ret26 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));    -- OK.
    end Func26;

    function Func27 return F750A00.Lim_Comp is
    begin
       return Ret27 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));    -- ERROR:
    end Func27;

    function Func28 return F750A00.Lim_Comp is
    begin
       return Ret28 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));     -- ERROR:
    end Func28;

    function Func29 return F750A00.Lim_Comp is
    begin
       return Ret29 : F750A00.Lim_Comp :=
        (if B then (P => <>, N => 12)
              else F750A00.Lim_Comp(F750A00.Func_Lim_Comp));     -- ERROR:
    end Func29;


    function Func31 return F750A00.Lim_Ext is
    begin
       return Ret31 : F750A00.Lim_Ext :=
        (if B then F750A00.Func_Lim_Ext                          -- OK.
              else (R => (12, False), G => 87, N => 27));
    end Func31;

    function Func32 return F750A00.Lim_Ext is
    begin
       return Ret32 : F750A00.Lim_Ext :=
        (if B then (if B and True then F750A00.Func_Lim_Ext
                    else F750A00.Cnst_Lim_Ext)                   -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    end Func32;

    function Func33 return F750A00.Lim_Ext is
    begin
       return Ret33 : F750A00.Lim_Ext :=
        (if B then F750A00.Var_Lim_Ext                           -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    end Func33;

    function Func34 return F750A00.Lim_Rec is
    begin
       return Ret34 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Func_Lim_Ext.R) do                    -- ERROR:
          Ret34.B := True;
       end return;
    end Func34;

    function Func35 return F750A00.Lim_Rec is
    begin
       return Ret35 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else (F750A00.Func_Lim_Ext.R));                    -- ERROR:
    end Func35;

    function Func36 return F750A00.Lim_Rec is
    begin
       return Ret36 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));    -- ERROR:
    end Func36;

    function Func37 return F750A00.Lim_Rec is
    begin
       return Ret37 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Cnst_Lim_Ext.R);                      -- ERROR:
    end Func37;

    function Func38 return F750A00.Lim_Rec is
    begin
       return Ret38 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else (F750A00.Cnst_Lim_Ext.R));                    -- ERROR:
    end Func38;

    function Func39 return F750A00.Lim_Rec is
    begin
       return Ret39 : F750A00.Lim_Rec :=
        (if B then (52, False)
              else F750A00.Lim_Rec'(F750A00.Cnst_Lim_Ext.R));    -- ERROR:
    end Func39;

    function Func40 return F750A00.Lim_Ext is
    begin
       return Ret40 : F750A00.Lim_Ext :=
        (if B then (if not B then F750A00.Func_Lim_Ext
                   else Ren41)                                   -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    end Func40;

    function Func41 return F750A00.Lim_Ext is
    begin
       return Ret41 : F750A00.Lim_Ext :=
        (if B then (Ren41)                                       -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    end Func41;

    function Func42 return F750A00.Lim_Ext is
    begin
       return Ret42 : F750A00.Lim_Ext :=
        (if B then F750A00.Lim_Ext'(Ren41)                       -- ERROR:
              else (R => (12, False), G => 87, N => 27));
    end Func42;


    function Func43 return F750A00.Lim_Tagged'Class is
    begin
       return Ret43 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
         elsif not B then F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False)))
              else F750A00.Func_Lim_Tagged (False));             -- OK.
    end Func43;

    function Func44 return F750A00.Lim_Tagged'Class is
    begin
       return Ret44 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Func_Lim_Tagged_Access (False).all);  -- ERROR:
    end Func44;

    function Func45 return F750A00.Lim_Tagged'Class is
    begin
       return Ret45 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else (F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:
    end Func45;

    function Func46 return F750A00.Lim_Tagged'Class is
    begin
       return Ret46 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else  F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:
    end Func46;

    function Func47 return F750A00.Lim_Tagged'Class is
    begin
       return Ret47 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Obj_Any_Tagged_Access.all);           -- ERROR:
    end Func47;

    function Func48 return F750A00.Lim_Tagged'Class is
    begin
       return Ret48 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else (F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:
    end Func48;

    function Func49 return F750A00.Lim_Tagged'Class is
    begin
       return Ret49 : F750A00.Lim_Tagged'Class :=
        (if B then F750A00.Func_Lim_Tagged (True)
              else F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:
    end Func49;


    -- Case expressions:
    function Func51 return F750A00.Lim_Rec is
    begin
       return Ret51 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => F750A00.Func_Lim_Rec,
           when 4 .. 5 => F750A00.Func_Lim_Rec);                 -- OK.
    end Func51;

    function Func52 return F750A00.Lim_Rec is
    begin
       return Ret52 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Rec,
            when others => F750A00.Cnst_Lim_Rec);                -- ERROR:
    end Func52;

    function Func53 return F750A00.Lim_Rec is
    begin
       return Ret53 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 2 => F750A00.Var_Lim_Rec,                  -- ERROR:
            when 3 .. 5 => F750A00.Func_Lim_Rec);
    end Func53;

    function Func54 return F750A00.Lim_Rec is
    begin
       return Ret54 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Rec,
            when 4 .. 5 => (F750A00.Func_Lim_Rec));              -- OK.
    end Func54;

    function Func55 return F750A00.Lim_Rec is
    begin
       return Ret55 : F750A00.Lim_Rec :=
        (case V is
           when 1 => F750A00.Func_Lim_Rec,
           when 2 => (F750A00.Cnst_Lim_Rec),                     -- ERROR:
           when others => (12, B)) do
          Ret55.B := True;
       end return;
    end Func55;

    function Func56 return F750A00.Lim_Rec is
    begin
       return Ret56 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => (F750A00.Var_Lim_Rec));                -- ERROR:
    end Func56;

    function Func57 return F750A00.Lim_Rec is
    begin
       return Ret57 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 4 => F750A00.Func_Lim_Rec,
           when 5 => F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));    -- OK.
    end Func57;

    function Func58 return F750A00.Lim_Rec is
    begin
       return Ret58 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 | 5 => (52, False),
           when 4 => F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));    -- ERROR:
    end Func58;

    function Func59 return F750A00.Lim_Rec is
    begin
       return Ret59 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));-- ERROR:
    end Func59;

    function Func60 return F750A00.Lim_Rec is
    begin
       return Ret60 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => (52, False),
           when 4 .. 5 => F750A00.Lim_Rec(F750A00.Func_Lim_Rec));-- ERROR:
    end Func60;

    function Func61 return F750A00.Lim_Rec is
    begin
       return Ret61 : constant F750A00.Lim_Rec :=
        (case V is
           when 1 .. 3 => F750A00.Func_Lim_Rec,
           when 4 .. 5 => Ren01);                                -- ERROR:
    end Func61;


--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--    function Func62 return F750A00.Lim_Rec is
--    begin
--       return Ret62 : constant F750A00.Lim_Rec :=
--        (case V is
--           when 1 .. 3 => F750A00.Func_Lim_Rec,
--           when 4 .. 5 => raise Program_Error);                  -- OK.
--    end Func62;
--
--    function Func63x return F750A00.Lim_Rec is
--    begin
--       return Ret63 : constant F750A00.Lim_Rec :=
--        (case V is
--           when 1 .. 3 => F750A00.Func_Lim_Rec,
--           when others => raise Program_Error);                  -- OK.
--    end Func63;
--*** End replace.

    function Func64 return F750A00.Lim_Array is
    begin
       return Ret64 : constant F750A00.Lim_Array :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Array,
            when 4 .. 5 => F750A00.Func_Lim_Array(1..2));        -- ERROR:
    end Func64;

    function Func65 return F750A00.Lim_Rec is
    begin
       return Ret65 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (87, True),
            when 4 .. 5 => F750A00.Func_Lim_Array(2));           -- ERROR:
    end Func65;

    function Func66 return F750A00.Lim_Rec is
    begin
       return Ret66 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (87, True),
            when others => (F750A00.Func_Lim_Array(2))) do       -- ERROR:
          Ret66.B := True;
       end return;
    end Func66;

    function Func67 return F750A00.Lim_Rec is
    begin
       return Ret67 : constant F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (87, True),
            when 4 .. 5 =>
                   F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))); -- ERROR:
    end Func67;

    function Func68 return F750A00.Lim_Rec is
    begin
       return Ret68 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when others =>
                   F750A00.Func_Lim_Tagged_Access (False).R);    -- ERROR:
    end Func68;

    function Func69 return F750A00.Lim_Rec is
    begin
       return Ret69 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Obj_Any_Tagged_Access.R);     -- ERROR:
    end Func69;


    function Func70 return F750A00.Lim_Comp is
    begin
       return Ret70 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => F750A00.Func_Lim_Comp);               -- OK.
    end Func70;

    function Func71 return F750A00.Lim_Comp is
    begin
       return Ret71 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others => F750A00.Cnst_Lim_Comp);               -- ERROR:
    end Func71;

    function Func72 return F750A00.Lim_Comp is
    begin
       return Ret72 : F750A00.Lim_Comp :=
        (case V is
            when 2 | 4 => (P => <>, N => 12),
            when others => F750A00.Var_Lim_Comp);                -- ERROR:
    end Func72;

    function Func73 return F750A00.Lim_Comp is
    begin
       return Ret73 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Func_Lim_Comp));             -- OK.
    end Func73;

    function Func74 return F750A00.Lim_Comp is
    begin
       return Ret74 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Cnst_Lim_Comp));             -- ERROR:
    end Func74;

    function Func75 return F750A00.Lim_Comp is
    begin
       return Ret75 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 => (F750A00.Var_Lim_Comp));              -- ERROR:
    end Func75;

    function Func76 return F750A00.Lim_Comp is
    begin
       return Ret76 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others =>
                   F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));    -- OK.
    end Func76;

    function Func77 return F750A00.Lim_Comp is
    begin
       return Ret77 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 =>
                   F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));    -- ERROR:
    end Func77;

    function Func78 return F750A00.Lim_Comp is
    begin
       return Ret78 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when 4 .. 5 =>
                   F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));     -- ERROR:
    end Func78;

    function Func79 return F750A00.Lim_Comp is
    begin
       return Ret79 : F750A00.Lim_Comp :=
        (case V is
            when 1 .. 3 => (P => <>, N => 12),
            when others =>
                   F750A00.Lim_Comp(F750A00.Func_Lim_Comp));     -- ERROR:
    end Func79;


    function Func81 return F750A00.Lim_Ext is
    begin
       return Ret81 : F750A00.Lim_Ext :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Ext,                 -- OK.
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    end Func81;

    function Func82 return F750A00.Lim_Ext is
    begin
       return Ret82 : F750A00.Lim_Ext :=
        (case V is
            when 1 .. 3 => (if B and True then F750A00.Func_Lim_Ext
                            else F750A00.Cnst_Lim_Ext),          -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    end Func82;

    function Func83 return F750A00.Lim_Ext is
    begin
       return Ret83 : F750A00.Lim_Ext :=
        (if B then
           (case V is
              when 1 .. 3 => F750A00.Var_Lim_Ext,                -- ERROR:
              when others => (R => (12, False), G => 87, N => 27))
         else F750A00.Func_Lim_Ext);
    end Func83;

    function Func84 return F750A00.Lim_Rec is
    begin
       return Ret84 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Func_Lim_Ext.R);              -- ERROR:
    end Func84;

    function Func85 return F750A00.Lim_Rec is
    begin
       return Ret85 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => (F750A00.Func_Lim_Ext.R));            -- ERROR:
    end Func85;

    function Func86 return F750A00.Lim_Rec is
    begin
       return Ret86 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when others =>
                   F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R)) do  -- ERROR:
          Ret86.B := True;
       end return;
    end Func86;

    function Func87 return F750A00.Lim_Rec is
    begin
       return Ret87 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => F750A00.Cnst_Lim_Ext.R);              -- ERROR:
    end Func87;

    function Func88 return F750A00.Lim_Rec is
    begin
       return Ret88 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 => (F750A00.Cnst_Lim_Ext.R));            -- ERROR:
    end Func88;

    function Func89 return F750A00.Lim_Rec is
    begin
       return Ret89 : F750A00.Lim_Rec :=
        (case V is
            when 1 .. 3 => (52, False),
            when 4 .. 5 =>
                   F750A00.Lim_Rec'(F750A00.Cnst_Lim_Ext.R));    -- ERROR:
    end Func89;

    function Func90 return F750A00.Lim_Ext is
    begin
       return Ret90 : F750A00.Lim_Ext :=
        (case V is
            when 1 .. 3 => (if not B then F750A00.Func_Lim_Ext
               else Ren41),                                      -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    end Func90;

    function Func91 return F750A00.Lim_Ext is
    begin
       return Ret91 : F750A00.Lim_Ext :=
        (if B then
           (case V is
              when 1 | 3 => (Ren41),                             -- ERROR:
              when 2 | 4 .. 5 => (R => (12, False), G => 87, N => 27))
         else F750A00.Func_Lim_Ext);
    end Func91;

    function Func92 return F750A00.Lim_Ext is
    begin
       return Ret92 : F750A00.Lim_Ext :=
        (case V is
            when 1 .. 3 => F750A00.Lim_Ext'(Ren41),              -- ERROR:
            when 4 .. 5 => (R => (12, False), G => 87, N => 27));
    end Func92;


    function Func93 return F750A00.Lim_Tagged'Class is
    begin
       return Ret93 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 => F750A00.Func_Lim_Tagged (True),
            when 2 => F750A00.Lim_Tagged'Class'((
                    F750A00.Func_Lim_Tagged (False))),
            when 3 .. 5 => F750A00.Func_Lim_Tagged (False));     -- OK.
    end Func93;

    function Func94 return F750A00.Lim_Tagged'Class is
    begin
       return Ret94 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 =>
                   F750A00.Func_Lim_Tagged_Access (False).all);  -- ERROR:
    end Func94;

    function Func95 return F750A00.Lim_Tagged'Class is
    begin
       return Ret95 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when others =>
                   (F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:
    end Func95;

    function Func96 return F750A00.Lim_Tagged'Class is
    begin
       return Ret96 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 =>  F750A00.Lim_Tagged'Class'(
                    F750A00.Func_Lim_Tagged_Access (True).all)); -- ERROR:
    end Func96;

    function Func97 return F750A00.Lim_Tagged'Class is
    begin
       return Ret97 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 => F750A00.Obj_Any_Tagged_Access.all);   -- ERROR:
    end Func97;

    function Func98 return F750A00.Lim_Tagged'Class is
    begin
       return Ret98 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when 4 .. 5 => (F750A00.Obj_Any_Tagged_Access.all)); -- ERROR:
    end Func98;

    function Func99 return F750A00.Lim_Tagged'Class is
    begin
       return Ret99 : F750A00.Lim_Tagged'Class :=
        (case V is
            when 1 .. 3 => F750A00.Func_Lim_Tagged (True),
            when others => F750A00.Lim_Tagged'Class'(
                    F750A00.Obj_Any_Tagged_Access.all));         -- ERROR:
    end Func99;

begin
    null;
end B750A11;
