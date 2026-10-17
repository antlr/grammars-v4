-- B433001
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
--
--*
--
-- OBJECTIVE:
--    Check that <> is not allowed in positional array aggregates other than
--    in an others choice.
--
-- TEST DESCRIPTION:
--    This objective is testing a syntax error. However, this error is
--    much more likely to occur in practice than most syntax errors, so we
--    test for it. Moreover, the complex grammar of aggregates suggests that
--    it is likely that many implementations will implement it as a
--    legality check, rather than as part of the syntax.
--
--    Illegal aggregates have been surrounded by correct code to ease
--    error recovery.
--
-- CHANGE HISTORY:
--    24 Aug 2007   RLB   Created test from similar record test (B431001).
--    19 Sep 2007   RLB   Repaired object names.
--    19 Jul 2012   RLB   Changed definition of Test_Array_2 and uses so
--                        aggregates are have correct dimensionality.
--!

procedure B433001 is

   type Rec is record
      A : Integer := 10;
      B : Boolean := True;
      C : Character := 'C';
   end record;
   Rec_1 : constant Rec := (others => <>);                    -- OK.
   Rec_2 : constant Rec := (5, False, 'A');                   -- OK.
   Rec_3 : constant Rec := (4, B => <>, C => 'F');            -- OK.

   type Test_Array_1 is array (Positive range <>) of Rec;
   type Test_Array_2 is array (Boolean range <>, Character range <>) of Rec;

begin
   declare
      O1 : Test_Array_1 := (Rec_1, Rec_2);                    -- OK.
      O2 : Test_Array_1 := (<>, Rec_3, Rec_2);                -- ERROR:
   begin
      if O1(1).A = 2 then
         null;
      end if;
   end;
   declare
      O3 : Test_Array_1(2..4) := (others => Rec_3);           -- OK.
      O4 : Test_Array_1(2..4) := (Rec_1, <>, others => Rec_2);-- ERROR:
   begin
      if O3(4).C = 'D' then
         null;
      end if;
   end;
   declare
      O5 : Test_Array_1(1..4) := (others => <>);              -- OK.
      O6 : Test_Array_1(1..4) := (<>, Rec_3, others => <>);   -- ERROR:
   begin
      if O5(1).A = 2 then
         null;
      end if;
   end;
   declare
      O7 : Test_Array_1 := (Rec_1, (4, True, C => <>), Rec_3);-- OK.
      O8 : Test_Array_1 := ((4, True, C => <>), Rec_3, <>);   -- ERROR:
   begin
      if O7(1).A = 2 then
         null;
      end if;
   end;
   declare
      O11: Test_Array_2 := ((Rec_1, Rec_2),(Rec_3, Rec_3));   -- OK.
      O12: Test_Array_2 := ((<>, Rec_1),(Rec_2, Rec_3));      -- ERROR:
   begin
      if O11(False,Character'First).A = 2 then
         null;
      end if;
   end;
   declare
      O13: Test_Array_2(Boolean, 'A'..'B') :=
            ((Rec_1, others =><>), (Rec_3, others => <>));    -- OK.
      O14: Test_Array_2(Boolean, 'A'..'B') :=
            ((<>, others => Rec_1), (Rec_3, others => Rec_2));-- ERROR:
   begin
      if O13(False,'A').A = 2 then
         null;
      end if;
   end;
   declare
      O15: Test_Array_2(Boolean, 'c'..'e') :=
            ((Rec_1, Rec_2, Rec_3), (Rec_3, Rec_2, Rec_1));   -- OK.
      O16: Test_Array_2(Boolean, 'c'..'e') :=
            ((Rec_1, Rec_2, Rec_3), (Rec_3, <>, Rec_1));      -- ERROR:
   begin
      if O15(False,'c').A = 2 then
         null;
      end if;
   end;
   declare
      O17: Test_Array_2(Boolean, 'x'..'z') :=
            ((Rec_1, Rec_2, Rec_3), (others => <>));          -- OK.
      O18: Test_Array_2(Boolean, 'x'..'z') :=
            ((Rec_1, Rec_2, <>), (others => <>));             -- ERROR:
   begin
      if O17(False,'z').A = 2 then
         null;
      end if;
   end;
end B433001;
