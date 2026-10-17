-- B431001
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
--    Check that a positional component association in a record aggregate
--    cannot have a <> rather than an expression.
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
--    26 Apr 2007   RLB   Created test (tried to use submitted test, but it
--                        did not test the objective).
--!

procedure B431001 is

   type Test_Record is record
      A : Integer;
      B : Boolean := True;
      C : Character := 'C';
   end record;

   type Singleton is record
      D : Duration := 0.0;
   end record;

begin
   declare
      O1 : Test_Record := (1, False, 'A');          -- OK.
      O2 : Test_Record := (<>, False, 'B');         -- ERROR:
   begin
      if O1.A = 2 then
         null;
      end if;
   end;
   declare
      O3 : Singleton := (D => <>);                  -- OK.
      O4 : Test_Record := (2, <>, 'B');             -- ERROR:
   begin
      if O3.D = 1.0 then
         null;
      end if;
   end;
   declare
      O5 : Test_Record := (2, True, C => <>);       -- OK.
      O6 : Test_Record := (2, True, <>);            -- ERROR:
   begin
      if O5.A = 2 then
         null;
      end if;
   end;
   declare
      O7 : Singleton := (D => 2.0);                 -- OK.
      O8 : Singleton := (<>);                       -- ERROR:
         -- (Also, not named notation in an aggregate with a single component.)
   begin
      if O7.D = 2.0 then
         null;
      end if;
   end;
   declare
      O9 : Test_Record := (2, True, others => <>);  -- OK.
      OA : Test_Record := (<>, True, others => <>); -- ERROR:
   begin
      if O9.A = 2 then
         null;
      end if;
   end;
   declare
      OB : Test_Record := (2, C => 'A', B => <>);   -- OK.
      OC : Test_Record := (2, C => 'A', <>);        -- ERROR:
         -- (Also, not positional notation after named notation.)
   begin
      if OB.A = 2 then
         null;
      end if;
   end;
end B431001;
