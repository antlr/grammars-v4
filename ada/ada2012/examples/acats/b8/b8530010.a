-- B8530010.A
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
--    Check that the name of a renamed limited view of a package cannot be
--    used outside of the scope of a with clause for the package or the
--    immediate scope of the renaming.
--
-- TEST DESCRIPTION:
--    The test checks illegal uses of names by 8.5.3(3.1/2).
--
--    For that to happen, we need a package A that is limited withed into a
--    second package B, and then the second package is with into a third
--    package C that does not with A (in any way). If B contains renames of
--    and A nested packages of A, the renames cannot be referenced in C.
--
--
--    In this case, we try C being a package specification as well as the
--    main subprogram. Since a limited with only makes types and packages
--    visible, that's all we put in package A.

-- TEST FILES:
--      This test consists of the following files:
--      -> B8530010.A
--         B8530011.A
--         B8530012.A
--         B8530013.A
--         B8530014.A
--
-- PASS/FAIL CRITERIA:
--      Files B8530013.A and B8530014.A contain errors. All errors in these
--      files must be detected to pass the test.
--
-- CHANGE HISTORY:
--    08 Jun 2018  RLB  Created test.
--
--!

package B853001A is
   type Huey is range 0 .. 12;

   package Inner is
      type Dewey is (A, B, C);

      package Innermost is
         type Louie is digits 5;
      end Innermost;

   end Inner;

end B853001A;
