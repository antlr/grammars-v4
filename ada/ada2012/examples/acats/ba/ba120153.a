-- BA120153.A
--
--                            Grant of Unlimited Rights
--
--    AdaCore holds unlimited rights in the software and documentation
--    contained herein. Unlimited rights are the same as those granted
--    by the U.S. Government for older parts of the Ada Conformity
--    Assessment Test Suite, and are defined in DFAR 252.227-7013(a)(19).
--    By making this public release, AdaCore intends to confer upon all
--    recipients unlimited rights equal to those held by the Ada Conformity
--    Assessment Authority. These rights include rights to use, duplicate,
--    release or disclose the released technical data and computer software
--    in whole or in part, in any manner and for any purpose whatsoever,
--    and to have or permit others to do so.
--
--                                   DISCLAIMER
--
--    ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--    DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--    TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--    DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--    DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--    PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                    Notice
--
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--
--    This test is based on one submitted by AdaCore; AdaCore retains the
--    copyright on the test.
--*
--
-- OBJECTIVE:
--    See BA120150.A.
--
-- TEST DESCRIPTION
--    See BA120150.A.
--
-- TEST FILES:
--    This test consists of the following files:
--        BA120150.A
--        BA120151.A
--        BA120152.A
--     -> BA120153.A
--        BA120154.A
--
-- PASS/FAIL CRITERIA:
--    See BA120150.A.
--
-- CHANGE HISTORY:
--    22 May 2004 JM  Initial Version.
--    29 Mar 2007 RLB Created ACATS test from submitted test; removed
--                    test cases already covered by BA12014.
--
--!

--  Test that a private withed unit (of any sort) cannot be used in the
--  public part of a package, but it can be used in its private part.

private with BA12015x.P; -- Private with of a sibling private package.
pragma Elaborate (BA12015x);                          -- OK.
pragma Elaborate (BA12015x.P);                        -- OK.
package BA12015x.Q is
   O_1 : BA12015x.P.Low_Level_Data := 0;              -- ERROR: Not visible.

private
   O_2 : BA12015x.P.Low_Level_Data                    -- OK.
           := BA12015x.P.Max_Value;                   -- ERROR: Not visible.

   use BA12015x.P;
   O_3 : Low_Level_Data                               -- OK.
           := Max_Value;                              -- ERROR: Not visible.
   O_4 : T_Hidden := null;                            -- ERROR: Not visible.
end BA12015x.Q;

