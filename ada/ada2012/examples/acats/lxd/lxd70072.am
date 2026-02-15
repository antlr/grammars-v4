-- LXD70072.AM
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
--      pragma Restrictions (No_Dynamic_Priorities) is included. Specifically 
--      when there is a semantic dependency on Ada.Dynamic_Priorities in a
--      package making up the partition
--
-- TEST DESCRIPTION:
--      Pragma Restrictions No_Dynamic_Priorities is called out in a separate
--      text file.  A library level package "withs" and makes calls to 
--      Ada.Dynamic_Priorities and is contained in a second file.  A main
--      unit which "withs"  the package is declared in a third file.  
--      These three files are  brought together in a single partition.
--      The method by which the files are linked together in a partition 
--      is implementation dependent.
--
-- TEST FILES:
--      This test consists of the following files:
--          LXD70070.A     A text file containing the Restrictions pragma.
--          LXD70071.A     A package which has calls to Dynamic_Priorities.
--       => LXD70072.AM    The main program.
--
-- PASS/FAIL CRITERIA:     
--      There are two acceptable results for this test:
--      (a)  A link time error indicating that an attempt is being 
--      made to include code which has semantic dependencies on the package
--      Dynamic_Priorities.
--
--      (b) A compile time error indicating that the attempt to include the
--      library unit has been rejected
--
--      The test FAILS if the main program is allowed to execute.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      02 Oct 96   SAIC    Multi-file prolog convention fix.
--
--!

with LXD70071;   -- include the calls to Dynamic_Priorities
with Report;
         
procedure LXD70072 is


begin -- LXD70072

   Report.Test ("LXD70072", "Tasking Restrictions: No_Dynamic_Priorities");



   LXD70071.Credit_Task.Input;

   Report.Failed ("Should not execute");

   Report.Result;

   abort LXD70071.Credit_Task;  -- to prevent the test from hanging

end LXD70072;
