-- LXH40012.AM
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
--         No_Protected_Types
--      disallows protected types in the units previously compiled into
--      the program library.
--
-- TEST DESCRIPTION:
--      Following are several scenarios for checking the correct processing
--      of "pragma Restrictions".  Hereincluded are all the forms of these
--      tests for comparative purposes.  Each test will duplicate the
--      structure definition for its own particular model.
--
--      This test is an example of test design "A".
--
--      Dashed lines indicate file boundaries.
--
--    A:
--      -----------------------------   This fails at link time.  (L test)
--      | Config pragma             |
--      - - - - - - - - - - - - - - -
--      | OK unit.                  |
--      - - - - - - - - - - - - - - -
--      | Unit that violates pragma | \
--      - - - - - - - - - - - - - - -  > may be combined into one file
--      | Main withs both units     | /
--      -----------------------------
--
--      Note that the above example may fail on compilation of the third file,
--      depending on what the compiler considers the "compilation environment"
--      to be. This shall be reflected in the applicability criteria for those
--      tests.
--
--    B:
--      -----------------------------   This fails at link time.  (L test)
--      | Unit that violates pragma |
--      - - - - - - - - - - - - - - -
--      | Config pragma             |
--      - - - - - - - - - - - - - - -
--      | OK unit.                  |
--      - - - - - - - - - - - - - - -
--      | Main withs both units     |
--      -----------------------------
--
--    C:
--      -----------------------------   This links and runs (C test)
--      | Config pragma             |
--      | OK Unit.                  |
--      - - - - - - - - - - - - - - -
--      | Unit that violates pragma |
--      - - - - - - - - - - - - - - -
--      | Main that only withs the  |
--      | unit that violates the    |
--      | pragma                    |
--      -----------------------------
--
--    D:
--      -----------------------------   This fails at compile time
--      | Config pragma             |   (B Test)
--      | Unit(s) that violate      |
--      | pragma                    |
--      -----------------------------
--
--    E:
--      -----------------------------   This fails at compile time
--      | Config pragma             |   (B Test)
--      | Unit(s) that violate      |
--      | pragma                    |
--      - - - - - - - - - - - - - - -
--      | Unit that violates pragma |   However, the second file should
--      -----------------------------   successfully compile
--
--
-- SPECIAL REQUIREMENTS:
--      This test must be built in a single partition.
--      To build this test:
--        1) Compile LXH40010.A
--        2) Compile LXH40011.A
--        3) Compile LXH40012.AM
--        4) Attempt to build an executable image: LXH40012
--        5) If an executable image results, run it.
--
-- TEST FILES:
--      This test consists of the following files:
--
--         LXH40010.A
--         LXH40011.A
--    =>   LXH40012.AM
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable for a compiler attempting validation
--      for the Safety and Security Annex.
--
-- PASS/FAIL CRITERIA:
--      The test passes if:
--        A compile time error is generated indicating that the restiction
--          has been violated.
--        A linker error is generated because the restriction has been
--          violated.
--      The test fails if:
--        An executable image is linked.
--
--
-- CHANGE HISTORY:
--      25 FEB 96   SAIC   Initial version
--      12 JUN 96   SAIC   Revised for more complete configuration pragma
--                         checking.
--      05 NOV 96   SAIC   Restructured for release 2.1
--      29 JUN 98   EDS    Changed main procedure name.
--      02 FEB 17   RLB    Corrected format of error tag (and added a location
--                         indicator).
--
--!

------------------------------------------------------------------- LXH40012

with LXH4001_0;
procedure LXH40012 is

  protected Should_Be_Illegal is                   -- OPTIONAL ERROR: {3}
    entry Passeren;     -- Protected types not allowed in this partition.
    entry Vrijgeven;    -- pragma Restrictions(No_Protected_Types) in LXH40010.
  private
    In_Use : Boolean := False;
  end Should_Be_Illegal;

  protected body Should_Be_Illegal is

    entry Passeren when not In_Use is
    begin
      In_Use := True;
    end Passeren;

    entry Vrijgeven when In_Use is
    begin
      In_Use := False;
    end Vrijgeven;

  end Should_Be_Illegal;

    procedure Prolagen renames Should_Be_Illegal.Passeren;
    procedure Verhogen renames Should_Be_Illegal.Vrijgeven;


begin  -- Main test procedure.

  Prolagen;

  Verhogen;

end LXH40012;
