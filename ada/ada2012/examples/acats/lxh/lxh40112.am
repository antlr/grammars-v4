-- LXH40112.AM
--
--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
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
--      Check that pragma Restrictions (using the restrictions defined
--      in Annex H) applies to all units in a partition.
--      Check that the application of the configuration pragma Restrictions
--      with the specific restriction:
--         No_Dispatch
--      disallows T'Class in the units previously compiled into
--      the program library.
--
-- TEST DESCRIPTION:
--      See LXH4012.AM for comparisons to related tests.
--
--      -----------------------------   This fails at link time.  (L test)
--      | Unit that violates pragma |
--      - - - - - - - - - - - - - - -
--      | Config pragma             |
--      - - - - - - - - - - - - - - -
--      | Main withs bad unit       |
--      -----------------------------
--
-- SPECIAL REQUIREMENTS:
--      This test must be built in a single partition.
--      To build this test:
--        1) Compile LXH40110.A
--        2) Compile LXH40111.A
--        3) Compile LXH40112.AM
--        4) Attempt to build an executable image: LXH40112
--        6) If an executable image results, run it.
--
-- TEST FILES:
--      This test consists of the following files:
--
--         LXH40110.A
--         LXH40111.A
--    =>   LXH40112.AM
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Safety and Security Annex.
--	This test is not applicable to an implementation which requires
--      Restrictions pragmas to be given in an empty partition; such an
--      an implementation may reject unit LXH40111.A.
--
-- PASS/FAIL CRITERIA:
--      The test passes if:
--        A linker error is generated because the restriction has been
--          violated.
--        A compile time error is generated indicating that the restiction
--          has been violated (as no legal partition can constructed containing
--          LXH40110.A).
--        NOTE: If compile time errors are generated indicating that the
--              restiction has been violated when compiling LXH40110.A,
--              then the configuration pragma has probably been processed
--              before compiling LXH40110.A, which is not the correct procedure.
--      The test fails if:
--        An executable image is linked.
--
--
-- CHANGE HISTORY:
--      22 MAR 96   SAIC   Initial version
--      05 NOV 96   SAIC   Restructured for release 2.1
--      29 JUN 98   EDS    Changed main program name.
--      06 AUG 98   EDS    Moved post-restriction errors to LXH4014.
--      26 DEC 00   RLB    Repaired Pass/Fail criteria.

--!

------------------------------------------------------------------- LXH40112

with LXH4011_0;
procedure LXH40112 is

begin

  LXH4011_0.TC_Check;

end LXH40112;
