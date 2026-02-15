-- BA120127.A
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
--     See BA120120.A.
--
-- TEST DESCRIPTION
--     See BA120120.A.
--
-- TEST FILES:
--     This test consists of the following files:
--         BA120120.A
--         BA120121.A
--         BA120122.A
--         BA120123.A
--         BA120124.A
--         BA120125.A
--         BA120126.A
--      -> BA120127.A
--         BA120128.A
--         BA120129.A
--         BA12012A.A
--
-- PASS/FAIL CRITERIA:
--     See BA120120.A.
--
-- CHANGE HISTORY:
--    28 Mar 2007   RLB   Created test, based on ACATS 2.0 test BA12004.
--
--!

-- Child of parent:

limited with BA12012_0.BA12012_1.BA12012_3;   -- OK: Private sibling.
limited with BA12012_0.BA12012_2;             -- OK: Private sibling of parent.
limited with BA12012_0.BA12012_2.BA12012_4;   -- OK: Private parent.
limited with BA12012_0.BA12012_2.BA12012_4.BA12012_5; -- ERROR: Private,
                                                      --        not descendant.
private package BA12012_0.BA12012_1.BA12012_T7 is

    type Fooey is null record;

end BA12012_0.BA12012_1.BA12012_T7;
