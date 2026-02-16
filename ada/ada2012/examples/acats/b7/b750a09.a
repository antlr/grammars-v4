-- B750A09.A
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
--     This is a Ada 2012 companion test to test B750A02, which tries
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
procedure B750A09 is

   B : Boolean := True;
   subtype Tiny is Integer range 1 .. 5;
   V : Tiny := 4;
   TBD_Error : exception;

   Ren01 : F750A00.Lim_Rec renames F750A00.Func_Lim_Rec;         -- OK.
   Ren41 : F750A00.Lim_Ext renames F750A00.Func_Lim_Ext;         -- OK.


   type Ginormous is record
      -- If expressions:
      Comp01 : F750A00.Lim_Rec :=
          (if B then F750A00.Func_Lim_Rec
                else F750A00.Func_Lim_Rec);                      -- OK.
      Comp02 : F750A00.Lim_Rec :=
          (if B then F750A00.Func_Lim_Rec
                else F750A00.Cnst_Lim_Rec);                      -- ERROR:
      Comp03 : F750A00.Lim_Rec :=
          (if B then F750A00.Var_Lim_Rec                         -- ERROR:
                else F750A00.Func_Lim_Rec);
      Comp04 : F750A00.Lim_Rec :=
          (if B then F750A00.Func_Lim_Rec
                else (F750A00.Func_Lim_Rec));                    -- OK.
      Comp05 : F750A00.Lim_Rec :=
          (if B then F750A00.Func_Lim_Rec
           elsif B then (F750A00.Cnst_Lim_Rec)                   -- ERROR:
           else (12, B));
      Comp06 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else (F750A00.Var_Lim_Rec));                     -- ERROR:
      Comp07 : F750A00.Lim_Rec :=
          (if B then F750A00.Func_Lim_Rec
                else F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));    -- OK.
      Comp08 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));    -- ERROR:
      Comp09 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));     -- ERROR:
      Comp10 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Lim_Rec(F750A00.Func_Lim_Rec));     -- ERROR:
      Comp11 : F750A00.Lim_Rec :=
          (if B then F750A00.Func_Lim_Rec
                else Ren01);                                     -- ERROR:

--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--      Comp12 : F750A00.Lim_Rec := (raise TBD_Error);             -- OK.
--      Comp13 : F750A00.Lim_Rec :=
--          (if B then F750A00.Func_Lim_Rec
--                else raise Program_Error);                       -- OK.
--*** End replace.

      Comp14 : F750A00.Short_Lim_Array :=
          (if B then F750A00.Func_Lim_Array
                else F750A00.Func_Lim_Array(1..3));              -- ERROR:
      Comp15 : F750A00.Lim_Rec :=
          (if B then (87, True)
                else F750A00.Func_Lim_Array(2));                 -- ERROR:
      Comp16 : F750A00.Lim_Rec :=
          (if B then (87, True)
                else (F750A00.Func_Lim_Array(2)));               -- ERROR:
      Comp17 : F750A00.Lim_Rec :=
          (if B then (87, True)
                else
                   F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))); -- ERROR:
      Comp18 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Func_Lim_Tagged_Access (False).R);  -- ERROR:
      Comp19 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Obj_Any_Tagged_Access.R);           -- ERROR:

      Comp20 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else F750A00.Func_Lim_Comp);                     -- OK.
      Comp21 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else F750A00.Cnst_Lim_Comp);                     -- ERROR:
      Comp22 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else F750A00.Var_Lim_Comp);                      -- ERROR:
      Comp23 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else (F750A00.Func_Lim_Comp));                   -- OK.
      Comp24 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else (F750A00.Cnst_Lim_Comp));                   -- ERROR:
      Comp25 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else (F750A00.Var_Lim_Comp));                    -- ERROR:
      Comp26 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));  -- OK.
      Comp27 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));  -- ERROR:
      Comp28 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));   -- ERROR:
      Comp29 : F750A00.Lim_Comp :=
          (if B then (P => <>, N => 12)
                else F750A00.Lim_Comp(F750A00.Func_Lim_Comp));   -- ERROR:

      Comp31 : F750A00.Lim_Ext :=
          (if B then F750A00.Func_Lim_Ext                        -- OK.
                else (R => (12, False), G => 87, N => 27));
      Comp32 : F750A00.Lim_Ext :=
          (if B then (if B and True then F750A00.Func_Lim_Ext
                      else F750A00.Cnst_Lim_Ext)                 -- ERROR:
                else (R => (12, False), G => 87, N => 27));
      Comp33 : F750A00.Lim_Ext :=
          (if B then F750A00.Var_Lim_Ext                         -- ERROR:
                else (R => (12, False), G => 87, N => 27));
      Comp34 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Func_Lim_Ext.R);                    -- ERROR:
      Comp35 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else (F750A00.Func_Lim_Ext.R));                  -- ERROR:
      Comp36 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));  -- ERROR:
      Comp37 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Cnst_Lim_Ext.R);                    -- ERROR:
      Comp38 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else (F750A00.Cnst_Lim_Ext.R));                  -- ERROR:
      Comp39 : F750A00.Lim_Rec :=
          (if B then (52, False)
                else F750A00.Lim_Rec'(F750A00.Cnst_Lim_Ext.R));  -- ERROR:
      Comp40 : F750A00.Lim_Ext :=
          (if B then (if not B then F750A00.Func_Lim_Ext
                     else Ren41)                                 -- ERROR:
                else (R => (12, False), G => 87, N => 27));
      Comp41 : F750A00.Lim_Ext :=
          (if B then (Ren41)                                     -- ERROR:
                else (R => (12, False), G => 87, N => 27));
      Comp42 : F750A00.Lim_Ext :=
          (if B then F750A00.Lim_Ext'(Ren41)                     -- ERROR:
                else (R => (12, False), G => 87, N => 27));

      Comp43 : F750A00.Lim_Ext :=
          (if B then F750A00.Func_Lim_Ext
           else F750A00.Lim_Ext'(
                      F750A00.Func_New_One));                    -- OK.
      Comp44 : F750A00.Lim_Ext :=
          (if B then F750A00.Func_New_One
                else F750A00.Func_Lim_Ext_Access.all);           -- ERROR:
      Comp45 : F750A00.Lim_Ext :=
          (if B then F750A00.Func_New_One
                else
                   (F750A00.Func_Lim_Ext_Access.all));           -- ERROR:
      Comp46 : F750A00.Lim_Ext :=
          (if B then F750A00.Func_New_One
                else F750A00.Lim_Ext'(
                     F750A00.Func_Lim_Ext_Access.all));          -- ERROR:
      Comp47 : F750A00.Lim_Ext :=
          (if B then F750A00.Func_New_One
                else F750A00.Obj_Lim_Ext_Access.all);            -- ERROR:
      Comp48 : F750A00.Lim_Ext :=
          (if B then F750A00.Func_New_One
                else (F750A00.Obj_Lim_Ext_Access.all));          -- ERROR:
      Comp49 : F750A00.Lim_Ext :=
          (if B then F750A00.Func_New_One
                else F750A00.Lim_Ext'(
                      F750A00.Obj_Lim_Ext_Access.all));          -- ERROR:

      -- Case expressions:
      Comp51 : F750A00.Lim_Rec :=
          (case V is
             when 1 .. 3 => F750A00.Func_Lim_Rec,
             when 4 .. 5 => F750A00.Func_Lim_Rec);               -- OK.
      Comp52 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => F750A00.Func_Lim_Rec,
              when others => F750A00.Cnst_Lim_Rec);              -- ERROR:
      Comp53 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 2 => F750A00.Var_Lim_Rec,                -- ERROR:
              when 3 .. 5 => F750A00.Func_Lim_Rec);
      Comp54 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => F750A00.Func_Lim_Rec,
              when 4 .. 5 => (F750A00.Func_Lim_Rec));            -- OK.
      Comp55 : F750A00.Lim_Rec :=
          (case V is
             when 1 => F750A00.Func_Lim_Rec,
             when 2 => (F750A00.Cnst_Lim_Rec),                   -- ERROR:
             when others => (12, B));
      Comp56 : F750A00.Lim_Rec :=
          (case V is
             when 1 .. 3 => (52, False),
             when 4 .. 5 => (F750A00.Var_Lim_Rec));              -- ERROR:
      Comp57 : F750A00.Lim_Rec :=
          (case V is
             when 1 .. 4 => F750A00.Func_Lim_Rec,
             when 5 => F750A00.Lim_Rec'(F750A00.Func_Lim_Rec));  -- OK.
      Comp58 : F750A00.Lim_Rec :=
          (case V is
             when 1 .. 3 | 5 => (52, False),
             when 4 => F750A00.Lim_Rec'(F750A00.Cnst_Lim_Rec));  -- ERROR:
      Comp59 : F750A00.Lim_Rec :=
          (case V is
             when 1 .. 3 => (52, False),
             when 4 .. 5 =>
                F750A00.Lim_Rec'(F750A00.Var_Lim_Rec));          -- ERROR:
      Comp60 : F750A00.Lim_Rec :=
          (case V is
             when 1 .. 3 => (52, False),
             when 4 .. 5 =>
                F750A00.Lim_Rec(F750A00.Func_Lim_Rec));          -- ERROR:
      Comp61 : F750A00.Lim_Rec :=
          (case V is
             when 1 .. 3 => F750A00.Func_Lim_Rec,
             when 4 .. 5 => Ren01);                              -- ERROR:

--*** Replace these cases (either as OK or ERROR) once the ARG decides.
--      Comp62 : F750A00.Lim_Rec :=
--          (case V is
--             when 1 .. 3 => F750A00.Func_Lim_Rec,
--             when 4 .. 5 => raise Program_Error);                -- OK.
--      Comp63 : F750A00.Lim_Rec :=
--          (case V is
--             when 1 .. 3 => F750A00.Func_Lim_Rec,
--             when others => raise Program_Error);                -- OK.
----*** End replace.

      Comp64 : F750A00.Short_Lim_Array :=
          (case V is
              when 1 .. 3 => F750A00.Func_Lim_Array,
              when 4 .. 5 => F750A00.Func_Lim_Array(1..3));      -- ERROR:
      Comp65 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (87, True),
              when 4 .. 5 => F750A00.Func_Lim_Array(2));         -- ERROR:
      Comp66 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (87, True),
              when others => (F750A00.Func_Lim_Array(2)));       -- ERROR:
      Comp67 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (87, True),
              when 4 .. 5 =>
                   F750A00.Lim_Rec'(F750A00.Func_Lim_Array(2))); -- ERROR:
      Comp68 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (52, False),
              when others =>
                   F750A00.Func_Lim_Tagged_Access (False).R);    -- ERROR:
      Comp69 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (52, False),
              when 4 .. 5 => F750A00.Obj_Lim_Ext_Access.R);      -- ERROR:

      Comp70 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when 4 .. 5 => F750A00.Func_Lim_Comp);             -- OK.
      Comp71 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when others => F750A00.Cnst_Lim_Comp);             -- ERROR:
      Comp72 : F750A00.Lim_Comp :=
          (case V is
              when 2 | 4 => (P => <>, N => 12),
              when others => F750A00.Var_Lim_Comp);              -- ERROR:
      Comp73 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when 4 .. 5 => (F750A00.Func_Lim_Comp));           -- OK.
      Comp74 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when 4 .. 5 => (F750A00.Cnst_Lim_Comp));           -- ERROR:
      Comp75 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when 4 .. 5 => (F750A00.Var_Lim_Comp));            -- ERROR:
      Comp76 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when others =>
                     F750A00.Lim_Comp'(F750A00.Func_Lim_Comp));  -- OK.
      Comp77 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when 4 .. 5 =>
                     F750A00.Lim_Comp'(F750A00.Cnst_Lim_Comp));  -- ERROR:
      Comp78 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when 4 .. 5 =>
                     F750A00.Lim_Comp'(F750A00.Var_Lim_Comp));   -- ERROR:
      Comp79 : F750A00.Lim_Comp :=
          (case V is
              when 1 .. 3 => (P => <>, N => 12),
              when others =>
                     F750A00.Lim_Comp(F750A00.Func_Lim_Comp));   -- ERROR:

      Comp81 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => F750A00.Func_Lim_Ext,               -- OK.
              when 4 .. 5 => (R => (12, False), G => 87, N => 27));
      Comp82 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => (if B and True then F750A00.Func_Lim_Ext
                              else F750A00.Cnst_Lim_Ext),        -- ERROR:
                when 4 .. 5 => (R => (12, False), G => 87, N => 27));
      Comp83 : F750A00.Lim_Ext :=
          (if B then
             (case V is
                when 1 .. 3 => F750A00.Var_Lim_Ext,              -- ERROR:
                when others => (R => (12, False), G => 87, N => 27))
           else F750A00.Func_Lim_Ext);
      Comp84 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (52, False),
              when 4 .. 5 => F750A00.Func_Lim_Ext.R);            -- ERROR:
      Comp85 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (52, False),
              when 4 .. 5 => (F750A00.Func_Lim_Ext.R));          -- ERROR:
      Comp86 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (52, False),
              when others =>
                     F750A00.Lim_Rec'(F750A00.Func_Lim_Ext.R));  -- ERROR:
      Comp87 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (52, False),
              when 4 .. 5 => F750A00.Cnst_Lim_Ext.R);            -- ERROR:
      Comp88 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (52, False),
              when 4 .. 5 => (F750A00.Cnst_Lim_Ext.R));          -- ERROR:
      Comp89 : F750A00.Lim_Rec :=
          (case V is
              when 1 .. 3 => (52, False),
              when 4 .. 5 =>
                     F750A00.Lim_Rec'(F750A00.Cnst_Lim_Ext.R));  -- ERROR:
      Comp90 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => (if not B then F750A00.Func_Lim_Ext
                 else Ren41),                                    -- ERROR:
              when 4 .. 5 => (R => (12, False), G => 87, N => 27));
      Comp91 : F750A00.Lim_Ext :=
          (if B then
             (case V is
                when 1 | 3 => (Ren41),                           -- ERROR:
                when 2 | 4 .. 5 => (R => (12, False), G => 87, N => 27))
           else F750A00.Func_Lim_Ext);
      Comp92 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => F750A00.Lim_Ext'(Ren41),            -- ERROR:
              when 4 .. 5 => (R => (12, False), G => 87, N => 27));

      Comp93 : F750A00.Lim_Ext :=
          (case V is
              when 1 => F750A00.Func_New_One,
              when 2 => F750A00.Lim_Ext'((F750A00.Func_Lim_Ext)),
              when 3 .. 5 => F750A00.Func_New_One);              -- OK.
      Comp94 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => F750A00.Func_New_One,
              when 4 .. 5 =>
                   F750A00.Func_Lim_Ext_Access.all);             -- ERROR:
      Comp95 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => F750A00.Func_New_One,
              when others =>
                   (F750A00.Func_Lim_Ext_Access.all));           -- ERROR:
      Comp96 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => F750A00.Func_New_One,
              when 4 .. 5 => F750A00.Lim_Ext'(
                     F750A00.Func_Lim_Ext_Access.all));          -- ERROR:
      Comp97 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => F750A00.Func_New_One,
              when 4 .. 5 => F750A00.Obj_Lim_Ext_Access.all);    -- ERROR:
      Comp98 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => F750A00.Func_New_One,
              when 4 .. 5 =>
                          (F750A00.Obj_Lim_Ext_Access.all));     -- ERROR:
      Comp99 : F750A00.Lim_Ext :=
          (case V is
              when 1 .. 3 => F750A00.Func_New_One,
              when others => F750A00.Lim_Ext'(
                      F750A00.Obj_Lim_Ext_Access.all));          -- ERROR:
   end record;

begin
    null;
end B750A09;
