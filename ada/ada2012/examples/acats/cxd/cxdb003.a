-- CXDB003.A
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
--      Check that if a task becomes Held while waiting in a selective
--      accept and an entry call is issued to one of the open entries, the
--      corresponding accept body executes.  Check that once the rendezvous
--      completes the task does not execute until another Continue.
--
-- TEST DESCRIPTION:
--      Target_Task is a task with a selective accept.  The test driver
--      starts Target_Task and allows it to proceed to the Select.   It
--      then Holds Target_Task and, when held, calls an entry in the Select.
--      Once the driver has proved that code outside the rendezvous has
--      not been executed and that the rendezvous itself has completed
--      it Continues Target_Task and, by calling another entry in the
--      select proves that Target_Task is functioning properly.
--      This test will hang if the first rendezvous is not completed or
--      if Target_Task is not properly Continued.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable to implementations supporting:
--         the Real-Time Annex and
--         the package Asynchronous_Task_Control.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      09 Nov 95   SAIC    ACVC 2.0.1 - added applicability criteria
--      03 Feb 17   RLB     Added missing N/A error tag.
--
--!

with Report;
with ImpDef;
with Ada.Task_Identification;
with Ada.Asynchronous_Task_Control;         -- N/A => ERROR. {1}

procedure CXDB003 is

   package AATC renames Ada.Asynchronous_Task_Control;
   package ATI  renames Ada.Task_Identification;

begin

   Report.Test ("CXDB003", "Asynchronous Task Control. " &
                                       "Task is held while at an accept");
   declare -- encapsulate the test

      protected TC_PO is
         function Target_Executed return Boolean;
         function Target_Read_Complete return Boolean;
         procedure Set_Target_Executed;
         procedure Set_Target_Read_Complete;
      private
         Executed      : Boolean := false;
         Read_Complete : Boolean := false;
      end TC_PO;


      protected body TC_PO is

         function Target_Executed return Boolean is
         begin
            return Executed;
         end Target_Executed;

         function Target_Read_Complete return Boolean is
         begin
            return Read_Complete;
         end Target_Read_Complete;

         procedure Set_Target_Executed is
         begin
            Executed := true;
         end Set_Target_Executed;

         procedure Set_Target_Read_Complete is
         begin
            Read_Complete := true;
         end Set_Target_Read_Complete;

      end TC_PO;

      --=========================

      Target_Task_Id : ATI.Task_Id;

      task Target_Task is
         entry Read;
         entry Write;
         entry Get_Id(Task_Id : out ATI.Task_Id);
      end Target_Task;

      task body Target_Task is
      begin
         accept Get_Id (Task_Id : out ATI.Task_Id)  do
            -- Get the system assigned Task_Id for this task and
            -- "return" it to the caller
            Task_Id := ATI.Current_Task;
         end Get_Id;

         loop
            select

               accept Read do
                  TC_PO.Set_Target_Read_Complete;
               end Read;
               -- This should not be executed till the task is Continued
               TC_PO.Set_Target_Executed;

            or

               accept Write do
                  null;
               end Write;

            or
               terminate;
            end select;

         end loop;

      exception
         when others =>
            Report.Failed ("Unexpected exception in Target_Task");
      end Target_Task;

   begin -- encapsulation

      -- Get the task's Id for the Hold/Continue instructions and start
      -- the Target_Task
      Target_Task.Get_Id (Target_Task_Id);

      -- Switch to allow Target_Task to get to the Selective_Accept
      delay ImpDef.Switch_To_New_Task;

      AATC.Hold (Target_Task_Id);
      -- Other tests in the suite are checking to ensure that the "Hold"
      -- takes effect properly.

      -- Call an entry in the Selective_Accept of Target_Task.  The
      -- rendezvous should complete.  The test will hang here if it does
      -- not do so.
      Target_Task.Read;

      -- Delay to allow Target_Task to (erroneously) proceed beyond the
      -- rendezvous
      delay ImpDef.Switch_To_New_Task;
      --
      if TC_PO.Target_Executed then
         Report.Failed ("Target_Task executed outside rendezvous");
      end if;

      -- Check that the rendezvous was completed (not cancelled)
      if not TC_PO.Target_Read_Complete then
         Report.Failed ("Read was not completed");
      end if;

      -- Now continue Target_Task and check that another entry will be handled
      AATC.Continue (Target_Task_Id);
      --
      Target_Task.Write;
      -- If the Continue does not allow Target_Task to proceed the test
      -- will hang here
      while not TC_PO.Target_Executed loop
         delay ImpDef.Minimum_Task_Switch;
      end loop;

   end;  -- encapsulation

   Report.Result;

end CXDB003;
