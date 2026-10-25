-- B8530013.A
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
--      See B8530010.A.
--
-- TEST DESCRIPTION:
--      See B8530010.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         B8530010.A
--      -> B8530011.A
--         B8530013.A
--         B8530013.A
--         B8530014.A
--
-- PASS/FAIL CRITERIA:
--      See B8530010.A.
--
-- CHANGE HISTORY:
--    08 Jun 2018  RLB  Created test.
--
--!

with B853001B;
package B853001C is

   -- We're not in the scope of any kind of clause for the renamed package.

   type Fooey is access B853001B.Ren_A.Huey;               -- ERROR: {25;1}

   type Blooey is access B853001B.Ren_A_Innermost.Louie;   -- ERROR: {26;1}

   procedure Flub (A : in B853001B.Ren_A.Huey;             -- ERROR: {27;1}
                   B : in Integer;
                   C : out B853001B.Ren_A_Inner.Dewey);    -- ERROR: {28;1}

   package Bad_Ren renames B853001B.Ren_A_Inner;           -- ERROR: {28;1}

end B853001C;
