-- LXD70032.AM
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
--      Check that a partition obeys the restriction if a configuration 
--      pragma Restrictions (No_Abort_Statements) is included. Specifically
--      a task with an abort_statement is not allowed.  
--
-- TEST DESCRIPTION:
--      Pragma Restrictions No_Abort_Statements is called out in a separate
--      text file.  A library level package contains a task which has
--      an abort statement is declared in another file.   A main unit which 
--      "withs"  the package is declared in a third file.  These three files 
--      are brought together in a single partition.  The method by which the
--      files are linked together in a partition is implementation dependent.
--
-- TEST FILES: 
--      This test consists of the following files:
--          LXD70030.A     A text file containing the Restrictions pragma
--          LXD70031.A     A Library task containing an Abort statement
--          LXD70032.AM    The main program
--
-- PASS/FAIL CRITERIA:     
--      There are two acceptable results for this test:
--      (a)  A link time error indicating that an attempt is being 
--      made to include a task which has an abort statement when the partition
--      has a Restriction of No_Abort_Statements.
--
--      (b) A compile time error indicating that the attempt to include the
--      library unit has been rejected
--
--      The test FAILS if the main program is allowed to execute.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      10 Nov 95   SAIC    ACVC 2.0.1 - fixed spelling of pragma
--      30 Sep 96   SAIC    Multi-file prolog convention fix.
--
--!

with LXD70031;   -- include the task with the abort_statement
with Report;
         
procedure LXD70032 is


begin -- LXD70032

   Report.Test ("LXD70032", "Tasking Restrictions: No_Abort_Statements");

   Report.Failed ("Should not execute");

   -- abort the tasks so the test does not hang
   LXD70031.Close_Control.Disaster_Shut_Down;

   Report.Result;

end LXD70032;
