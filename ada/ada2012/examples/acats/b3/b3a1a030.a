-- B3A1A030.A
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
-- OBJECTIVE:
--
--     Check that the name of an incomplete view cannot be used in a
--     use type clause.
--
-- TEST DESCRIPTION:
--
--     This test was inspired by AC-0163. File B3A1A030 checks uses
--     within a program unit; file B3A1A031 checks uses within a
--     context clause.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> B3A1A030.A
--         B3A1A031.A
--
-- PASS/FAIL CRITERIA:
--     All test files contain errors. All errors in each of file must be
--     detected.
--
-- CHANGE HISTORY:
--     30 May 2008  RLB  Created test.
--     07 Nov 2008  RLB  Corrected test objective.
--
limited with F3A1A00;
package B3A1A030 is

    use type F3A1A00.An_Untagged_Type;           -- ERROR:
    use type F3A1A00.A_Tagged_Type;              -- ERROR:
    use type F3A1A00.An_Access_to_Untagged;      -- ERROR:
    use type F3A1A00.An_Access_to_Tagged;        -- ERROR:
    use type F3A1A00.Untagged_Private;           -- ERROR:
    use type F3A1A00.Tagged_Private;             -- ERROR:

    type Untagged_Inc;

    type Tagged_Inc is tagged;

    use type Untagged_Inc;                       -- ERROR:
    use type Tagged_Inc;                         -- ERROR:

    type Untagged_Inc is range 1 .. 100;

    type Tagged_Inc is tagged null record;

    use type Untagged_Inc;                       -- OK.
    use type Tagged_Inc;                         -- OK.

end B3A1A030;
