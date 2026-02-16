-- B3A20174.A
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
--     See B3A20170.A.
--
-- TEST DESCRIPTION
--     See B3A20170.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B3A20170.A
--         B3A20171.A
--         B3A20172.A
--         B3A20173.A
--      -> B3A20174.A
--
-- PASS/FAIL CRITERIA:
--     See B3A20170.A.
--
-- CHANGE HISTORY:
--     18 Dec 2014  RLB  Created B-Test from submitted test.
--     13 Mar 2015  RLB  Eliminated overlong lines.
--!

with B3A2017_G.C;
with B3A2017_FP;
with B3A2017_Global;
with Ada.Text_IO;
procedure B3A20174 is

  procedure Bar is
    package I is new B3A2017_G(B3A2017_Global.Ref);
                         -- Store a reference to I.Foo in B3A2017_Global.P.
    package J is new I.C;
                         -- and store another one in B3A2017_Global.Q.
    package K is new B3A2017_FP(I);
                         -- and store another one in B3A2017_Global.R.
  begin
    null;
  end Bar;

begin
  Ada.Text_IO.Put_Line ("-- B3A2017 - Create dangling procedure " &
                                     "references (AI-229)");
  Bar;
  B3A2017_Global.P.all; -- Oops, I.X is gone?
  B3A2017_Global.Q.all; -- ditto
  B3A2017_Global.R.all; -- ditto
  Ada.Text_IO.Put_Line ("-- B3A2017 finished");
end B3A20174;

