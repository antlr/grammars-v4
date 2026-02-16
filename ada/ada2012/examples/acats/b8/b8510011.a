-- B8510011.A
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
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--    See B8510010.A.
--
-- TEST FILES:
--    This test consists of the following files:
--         B8510010.A
--      -> B8510011.A
--         B8510012.AM
--
-- PASS/FAIL CRITERIA:
--    See B8510010.A.
--
-- CHANGE HISTORY:
--    25 JAN 2001   PHL   Initial version.
--    29 JUN 2001   RLB   Reformatted for ACATS. Split files.
--
--!

package body B851001_0 is

    package body G3 is
	C1_Ren : Integer renames X.C1; -- ERROR: Decendant of untagged
                                       --        formal derived type.
    end G3;

    package body G4 is
	C1_Ren : Integer renames X.C1; -- ERROR: X is definite.
    end G4;

end B851001_0;

