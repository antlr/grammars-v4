-- LXH40093.AM
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
--         No_Access_Subprograms
--      disallows access to subprograms in the units previously compiled into
--      the program library.
--
-- TEST DESCRIPTION:
--      See LXH4012.AM for comparisons to related tests.
--
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
-- SPECIAL REQUIREMENTS:
--      This test must be built in a single partition.
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--      To build this test:
--        1) Compile LXH40090.A
--        2) Compile LXH40091.A
--        3) Compile LXH40092.A
--        4) Compile LXH40093.AM
--        5) Attempt to build an executable image: LXH4009
--        6) If an executable image results, run it.
--
-- TEST FILES:
--      This test consists of the following files:
--
--         LXH40090.A
--         LXH40091.A
--         LXH40092.A
--    =>   LXH40093.AM
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
--      22 MAR 96   SAIC   Initial version
--      05 NOV 96   SAIC   Restructured for release 2.1
--      29 JUN 98   EDS    Changed main program name.
--!

----------------------------------------------------------------- LXH40093

with LXH4009_0;
with LXH4009_1;
procedure LXH40093 is

  use type LXH4009_0.Access_Float;

  Func_E    : LXH4009_1.Access_Function;
  Root_Beer : LXH4009_0.Access_Float := LXH4009_0.Func( 21.0 * 2.0 );

  Failed_Test : exception;

begin

  if Root_Beer = null then
    raise Failed_Test;
  end if;

end LXH40093;
