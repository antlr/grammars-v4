-- BA120154.A
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
--        BA120153.A
--     -> BA120154.A
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

--  Check that a private with in the body has visible all the entities that are
--  made visible by a private_with clause placed in its package specification,
--  and that it is inherited by the subunit:

separate (BA12015x.R)
procedure Our_Proc is
   O_2 : BA12015x.P.Low_Level_Data                    -- OK.
           := BA12015x.P.Max_Value;                   -- ERROR: Not visible.

   use BA12015x.P;
   O_3 : Low_Level_Data                               -- OK.
           := Max_Value;                              -- ERROR: Not visible.
   O_4 : T_Hidden := null;                            -- ERROR: Not visible.
begin
    null;
end Our_Proc;

