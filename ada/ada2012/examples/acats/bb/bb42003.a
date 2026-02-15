-- BB42003.A
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
--     Check that a pragma Assertion_Policy is illegal if the policy is
--     not recognized.
--
-- TEST DESCRIPTION:
--     We follow the great tradition of the ACATS (see tests B28001A-B28001W),
--     and use the names of editors of the Ada Standard as policy names.
--     These aren't meaningful policies. We have to resort to this scheme
--     as we outwise would require some sort of textual substitution to
--     come up with a not-a-policy-name, and we do not want to create any
--     more use of the macro processor.
--
--     This test only checks the policy name as defined in Amendment 1; the
--     Ada 2012 assertion aspect marks will be tested separately.
--
-- CHANGE HISTORY:
--      25 Jan 16   RLB     Created test.
--      28 Mar 16   RLB     Added error location codes.
--
--!
procedure BB42003 is
   pragma Assertion_Policy (Ignore);                           -- OK. {4}

   pragma Assertion_Policy (S_Tucker_Taft);                    -- ERROR: {4}

   procedure Inner is

      pragma Assertion_Policy (Robert_A_Duff);                 -- ERROR: {7}

   begin
      declare
         pragma Assertion_Policy (Check);                      -- OK. {10}
      begin
         null;
      end;
   end Inner;

   package Nested is
      pragma Assertion_Policy (Pascal_Leroy);                  -- ERROR: {7}

      pragma Assertion_Policy (Check);                         -- OK. {7}

      Obj : Integer;
   end Nested;

begin
   null;
end BB42003;
