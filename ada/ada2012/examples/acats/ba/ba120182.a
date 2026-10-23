-- BA120182.A
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
--    See BA120180.A.
--
-- TEST DESCRIPTION
--    See BA120180.A.
--
-- TEST FILES:
--    This test consists of the following files:
--        BA120180.A
--        BA120181.A
--     -> BA120182.A
--        BA120183.A
--        BA120184.A
--        BA120185.A
--        BA120186.A
--        BA120187.A
--
-- PASS/FAIL CRITERIA:
--    See BA120180.A.
--
-- CHANGE HISTORY:
--    22 May 2018   RLB   Created test from existing test BA12014.
--
--!

private with BA12018_0.BA12018_2;
generic
package BA12018_T2 is

    X : Integer renames BA12018_0.Obj; -- ERROR: {25;1} Used in visible part.

    use BA12018_0;                     -- ERROR: { 9;1} Used in visible part.

    Y : BA12018_0.Rec;                 -- ERROR: { 9;1} Used in visible part.

private

    Z : constant Integer := BA12018_0.Obj + 1; -- OK. {29;5}

end BA12018_T2;
