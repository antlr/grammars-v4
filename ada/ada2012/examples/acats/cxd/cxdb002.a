-- CXDB002.A
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
--      Check that the effect of calling Get_Priority and Set_Priority
--      on a Held task is the same as on any other task.
--
-- TEST DESCRIPTION
--      The Target_Task waits at an accept statement while the test driver
--      checks the effects of Get/Set priority. These are used before,
--      during and after the Target_Task is Held.  Also the driver checks
--      that the priority which was set while the task was Held is carried
--      through after it is Continued.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable to implementations supporting:
--         the Real-Time Annex and
--         the package Asynchronous_Task_Control.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      09 Nov 95   SAIC    ACVC 2.0.1 fixed priority values
--      03 Feb 17   RLB     Added missing N/A error tag.
--
--!

with Report;
with System;
with Ada.Task_Identification;
with Ada.Asynchronous_Task_Control;         -- N/A => ERROR. {1}
with Ada.Dynamic_Priorities;

procedure CXDB002 is

   package AATC renames Ada.Asynchronous_Task_Control;
   package ATI  renames Ada.Task_Identification;
   package ADP  renames Ada.Dynamic_Priorities;

   Priority_1Q : constant System.Priority :=
                           (System.Priority'First + 5);
   Priority_3Q : constant System.Priority :=
                           (System.Default_Priority + 5);

begin -- CXDB002


   Report.Test ("CXDB002", "Asynchronous Task Control: Set/Get_Priority" &
                                                " on a Held task");
   declare -- encapsulate the test


      Target_Task_Id : ATI.Task_Id;

      Original_Priority : System.Priority;
      Held_Priority     : System.Priority;

      task Target_Task is
         pragma priority (Priority_1Q);
         entry Get_Id(Task_Id : out ATI.Task_Id);
         entry Shut_Down;
      end Target_Task;

      task body  Target_Task is
      begin
         accept Get_Id (Task_Id : out ATI.Task_Id)  do
            -- Get the system assigned Task_Id for this task and
            -- "return" it to the caller
            Task_Id := ATI.Current_Task;
         end Get_Id;

         -- Wait here so that the task does not terminate before the
         -- driver has completed the test.  The "Hold" will occur while
         -- we are at this accept statement
         accept Shut_Down;

      exception
         when others =>
               Report.Failed ("Unexpected Exception in Target_Task");
      end Target_Task;

   begin
      -- This is the test Driver

      -- Establish the Id of Target_Task
      Target_Task.Get_Id(Target_Task_Id);

      -- Check the effects of the Pragma Priority in the Target_Task
      Original_Priority := ADP.Get_Priority(Target_Task_Id);
      if Original_Priority /= Priority_1Q then
         Report.Failed ("Original Priority is incorrect");
      end if;

      -- Now Hold the Target_Task and check the effects while held
      AATC.Hold (Target_Task_Id);
      --
      -- We should be able to get the base priority of a Held task and it
      -- should not be affected.  Specifically it should NOT be the current
      -- active priority (which is lower than the null task)
      Held_Priority := ADP.Get_Priority (Target_Task_Id);
      if Held_Priority /= Original_Priority then
         Report.Failed ("Original Held Priority is incorrect");
      end if;

      -- We should be able to Set the base priority of a Held task
      --
      ADP.Set_Priority (Priority_3Q, Target_Task_Id);
      --
      Held_Priority := ADP.Get_Priority (Target_Task_Id);
      if Held_Priority /= Priority_3Q then
         Report.Failed ("New Held Priority is incorrect");
      end if;

      -- Now Continue the task and check that the priority that was
      -- set while it was Held is still in effect
      AATC.Continue (Target_Task_Id);
      --
      Held_Priority := ADP.Get_Priority (Target_Task_Id);
      if Held_Priority /= Priority_3Q then
         Report.Failed ("Priority of Continued task is incorrect");
      end if;

      -- The test is complete, allow the Target_Task to terminate
      Target_Task.Shut_Down;

   end;  -- encapsulation

   Report.Result;

end CXDB002;
