-- LXD70092.AM
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
--      Check that a partition obeys the restriction if the following
--      configuration restrictions are included:
--        pragma Restrictions (Max_Select_Alternatives => 0)
--        pragma Restrictions (Max_Task_Entries        => 0)
--        pragma Restrictions (Max_Protected_Entries   => 0)
--
-- TEST DESCRIPTION:
--      The pragma Restrictions are called out in a separate text file.
--      A library level package is defined which violates all three 
--      restrictions that we are trying to check.
--      A main procedure withs the library level package and causes
--      the restrictions to be violated at runtime.
--      After violating the restrictions the test prints failed.
--
-- TEST FILES:
--      This test consists of the following files:
--          LXD70090.A     A text file with the Restrictions pragma.
--          LXD70091.A     A package which violates the restrictions.
--       => LXD70092.AM    The main program.
--
-- PASS/FAIL CRITERIA:     
--      There are three acceptable results for this test:
--      (a)  A compile time error indicates that the specified restrictions
--           have been violated.  If all three restrictions are detected
--           at compile time then the test passes.  Otherwise, the test
--           must be split so that the violations that were not detected at
--           compile time can be shown to be detected after compile time.
--      (b)  A link time error indicating that the specified restrictions
--           have been violated.  Any restrictions not detected at compile
--           time must be detected at link or run time.
--      (c)  A run time error indicating that the specified restrictions
--           have been violated.  Any errors not detected at compile or 
--           link time must be detected at runtime.
--      The test FAILS if the main program executes and produces anything
--      except an indication that the specified restrictions have been
--      violated.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which are not
--      part of any Compilation Unit;  the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      28 SEP 95   SAIC    ACVC 2.1
--      30 Sep 96   SAIC    Multi-file prolog convention fix.
--
--!

with LXD7009_1;   -- the package which contains restrictions violations
with Report;
         
procedure LXD70092 is


begin -- LXD70092

   Report.Test ("LXD70092", "Tasking Restrictions:" &
                " Max_Select_Alternatives," &
                " Max_Task_Entries, and" &
                " Max_Protected_Entries");

   LXD7009_1.Violations_Abound;

   Report.Result;

end LXD70092;
