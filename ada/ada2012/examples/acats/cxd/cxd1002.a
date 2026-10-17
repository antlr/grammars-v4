-- CXD1002.A
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
--      Check that the base priority of the main subprogram can be set by
--      means of pragma priority.  
--      
--      Check that a task's base priority is the priority of the parent at the
--      time the task is created when the priority of the parent has been set
--      by means of pragma priority
--      
--      Check that a task's base priority is the priority of the parent at the
--      time the task is created when the priority of the grandfather has been
--      set by means of pragma priority
--      
-- TEST DESCRIPTION:
--      The main subprogram sets its priority to something other than the
--      default by means of pragma priority.  It then spawns a child, and the
--      child in turn spawns another child.  In each case check the base
--      priority with Ada.Dynamic_Priorities.Get_Priority.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      08 Nov 95   SAIC    ACVC 2.0.1
--
--!


with Report;
with System;
with Ada.Dynamic_Priorities;

procedure CXD1002 is

      Package ADP renames Ada.Dynamic_Priorities;
   
      Priority_1Q : constant System.Priority :=
                            (System.Priority'First + 5);

      -- Set the priority of the Main program fairly low to allow ample scope
      -- to vary the priority of spawned tasks.  
      -- Note: this is different from the default priority
      --
      Pragma Priority ( Priority_1Q );

begin

   Report.Test ("CXD1002", "Check setting of base priority of main " &
                     "with pragma priority");

   declare -- encapsulate the test

      task Line_Driver;
   
      task type Message_Task;
      type acc_Message_Task is access Message_Task;
   
   
      -- Simulate the Assembly of messages received from an external source
      -- The Line_Driver creates a message task for each
      --      Such a task would normally be designed to loop continuously
      --      creating the messages as input is received.  In this test 
      --      just create one dummy message task to check the base priority 
      --
      task body Line_Driver is
   
      begin
   
         -- This task was created by the Main subprogram which had set its 
         -- base priority by means of a pragma.  Check that this task has
         -- the same priority as the Main
         --
         if ADP.Get_Priority /= Priority_1Q then 
            Report.Failed ("Line_Driver's Priority incorrectly set");
         end if;
   
         -- At this point the Driver creates a Message_Task 
         declare
            N : acc_Message_Task := new Message_Task; 
         begin
            -- application code
            null; -- stub
   
         end;   -- declare
   
      exception
         when others => 
            Report.Failed ("Unexpected exception in Line_Driver");
      end Line_Driver;


      task body Message_Task is
      begin
         -- This task was created by a task whose base priority was set
         -- when its creator's priority was set by a pragma priority.  Check
         -- that the base priority is the same as the main (once removed).
         --
         if ADP.Get_Priority /= Priority_1Q then 
            Report.Failed ("Message_Task's Priority incorrectly set");
         end if;
         
         -- null;  perform the application code
   
      exception
         when others => 
            Report.Failed ("Unexpected exception in Message_Task");
   
      end Message_Task;



   begin -- declare block
      
      -- Check that the use of the pragma has set the base priority of the main
      -- subprogram to the expected value
      --
      if ADP.Get_Priority /= Priority_1Q then 
         Report.Failed ("Main's Priority incorrectly set");
      end if;

   end; -- test encapsulation
   
   Report.Result;

end CXD1002;
