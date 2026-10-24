-- B3A1A031.A
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
--     See B3A1A030.A.
--
-- TEST DESCRIPTION
--     See B3A1A030.A.
--
-- TEST FILES:
--     This test consists of the following files:
--         B3A1A030.A
--      -> B3A1A031.A
--
-- PASS/FAIL CRITERIA:
--     See B3A1A031.A.
--
-- CHANGE HISTORY:
--     30 May 2008  RLB  Created test based on example in AC-163.
--!

limited with F3A1A00.Child;
   -- F3A1A00 is mentioned but not named, that avoids 10.1.2(19).
   -- The following use_clauses name entities not in the declarative
   -- region of F3A1A00.Child, so the above is legal and the only
   -- error is from the illegal use of an incomplete view.
   -- "limited with F3A1A00;" would be illegal by 10.1.2(19) and
   -- thus would fail to test the objective.
use type F3A1A00.An_Untagged_Type;           -- ERROR:
use type F3A1A00.A_Tagged_Type;              -- ERROR:
use type F3A1A00.An_Access_to_Untagged;      -- ERROR:
use type F3A1A00.An_Access_to_Tagged;        -- ERROR:
use type F3A1A00.Untagged_Private;           -- ERROR:
use type F3A1A00.Tagged_Private;             -- ERROR:
package B3A1A031 is
    Obj1 : access F3A1A00.Untagged_Private;  -- OK.
    Obj2 : access F3A1A00.Tagged_Private;    -- OK.
end B3A1A031;
