-- BC510212.A
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
--*
--
-- OBJECTIVE
--      See BC510210.A.
--
-- TEST DESCRIPTION
--      See BC510210.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         BC510210.A
--         BC510211.A
--      -> BC510212.A
--         BC510213.AM
--
-- PASS/FAIL CRITERIA:
--      Files BC510211.A and BC510212.A contain errors. All
--      errors in these files must be detected to pass the test.
--
--
-- CHANGE HISTORY:
--      15 Jan 2012   GJD     Initial version.
--      25 Apr 2014   RLB     Split into two tests, this one for matching
--                            rules. Put units with errors into separate
--                            files.
--
--!

with BC51021_0;
with BC51021_1;
limited with BC51021_2;

package BC51021_4 is

   package Insts_For_Untagged_Incomplete is

      package Inst_1 is new BC51021_0 (BC51021_2.Unsigned_10);      -- OK.

      package Inst_2 is new BC51021_0 (BC51021_2.Fixed);            -- OK.

      package Inst_3 is new BC51021_0 (BC51021_2.Untagged_Record);  -- OK.

      package Inst_4 is new BC51021_0 (BC51021_2.Tagged_Record);    -- OK.

   end Insts_For_Untagged_Incomplete;

   package Insts_For_Tagged_Incomplete is

      package Inst_1 is new BC51021_1 (BC51021_2.Unsigned_10);      -- ERROR:

      package Inst_2 is new BC51021_1 (BC51021_2.Fixed);            -- ERROR:

      package Inst_3 is new BC51021_1 (BC51021_2.Untagged_Record);  -- ERROR:

      package Inst_4 is new BC51021_1 (BC51021_2.Tagged_Record);    -- OK.

   end Insts_For_Tagged_Incomplete;

end BC51021_4;


