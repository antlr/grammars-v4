-- CXD1001.A
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
--      Check that the range of System.Priority is at least 30 values; that
--      System.Interrupt_Priority has at least one value and is higher than
--      System.Priority and the System.Default_Priority is at the center of
--      the range of System.Priority. 
--
--      Check the behavior of  Ada.Dynamic_Priorities.Set_Priority and
--      Get_Priority; specifically that Set_Priority will set a value that can
--      later be confirmed with Get_Priority.   
--
--      Check that, in the absence of Pragma Priority, the main subprogram has
--      a base priority of Default_Priority.
--
-- TEST DESCRIPTION:
--      Verify the priority ranges with simple declarations and assignments.  
--      Include one additional runtime calculation.  Check the manipulation of
--      Ada.Dynamic_Priorities by getting the current value, setting it to 
--      something different then getting it again to check that the new value
--      has been set.  Note:  This test does not verify that the priority 
--      modification has any effect on the object program.
--
--      An incorrect range of system priorities will cause this to fail.
--      If the range of system priorities is < 30 the declaration of the
--      subtype App_Priority should result in a compile time warning or fatal
--      error.  If the fatal error does not occur then a Constraint_Error 
--      raised during the elaboration of the same declaration is a test 
--      failure.  If the compiler itself is incorrectly implemented
--      the assignment to App_Priority_1 should raise Constraint_Error
--      resulting in test failure.  The test is designed in such a way
--      that the exception is unhandled.  As a further check (which
--      does not rely on the implementation correctly dealing with
--      ranges) a "non-user" type check is used to verify the actual range.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      25 Apr 96   SAIC    ACVC 2.1
--
--!


with Report;
with System;
with Ada.Dynamic_Priorities;

procedure CXD1001 is

   Package ADP renames Ada.Dynamic_Priorities;

   Initial_Priority : System.Priority;
   New_Priority     : System.Priority;

   --This application uses the full range of the portable priorities
   subtype App_Priority is System.Priority
            range System.Priority'First..System.Priority'First+29;
            -- Note:  too small a range in System.Priority will cause 
            -- Constraint_Error during elaboration

   -- The application requires that the Most Urgent priority be labeled
   -- App_Priority_1 corresponding to "the first priority" and the 
   -- "least urgent" App_Priority_30
   App_Priority_30 : App_Priority := App_Priority'first;
   App_Priority_29 : App_Priority := App_Priority'first+1;
   --         ......     and so on
   --         ......     to
   App_Priority_1  : App_Priority := App_Priority'first+29;   
   -- Note:  too small a range in System.Priority will cause 
   -- Constraint_Error during elaboration

begin

   Report.Test ("CXD1001", "Check Priority range, set and get " &
                               "Ada.Dynamic_Priorities");

   -- Check the definitions in package System
   --
   if (System.Priority'Last - System.Priority'First) < 29 then
      Report.Failed ("Priority range is < 30");
      -- See comment in PASS/FAIL CRITERIA for the above line
   end if;
   --
   if System.Interrupt_Priority'First /= (System.Priority'Last + 1) then
      Report.Failed ("Interrupt_Priority'First is not correct");
   end if;
   --
   if System.Default_Priority /=
                        (System.Priority'First + System.Priority'Last)/2 then
      Report.Failed ("Default_Priority is not correct");
   end if;


   -- Now check the Get/Set of Dynamic Priorities
   --
   Initial_Priority := ADP.Get_Priority;
   --
   if Initial_Priority /= System.Default_Priority then
      Report.Failed ("Initial Priority incorrectly set");
   else
      ADP.Set_Priority (System.Priority'Last);
      New_Priority := ADP.Get_Priority;
      if New_Priority /= System.Priority'Last then
         Report.Failed ("New Priority incorrectly set");
      end if;
   end if;

   
   Report.Result;

end CXD1001;
