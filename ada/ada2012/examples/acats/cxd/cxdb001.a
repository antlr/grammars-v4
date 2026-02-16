-- CXDB001.A
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
--      Check that, in Ada.Asynchronous_Task_Control, the Hold operation
--      reduces the priority of the target task to such a state that it does
--      not  run and that Continue raises it such that it will run again.
--      Check that  Is_Held returns true if, and only if, the target task is
--      in the Held state. Check that Tasking_Error is raised if any of these
--      operations is applied to a task that is terminated.
--
-- TEST DESCRIPTION:
--      Two tasks are involved in this test.  Target_Task computes  in a
--      shared protected object.  Driver Holds Target_Task then Continues it,
--      verifying that Target_Task is actually halted or proceeding by
--      checking the state of Target_Task's on-going computation.  It also
--      verifies that Is_Held returns the expected state.  Driver then allows
--      Target_Task to terminate and checks that calls to
--      Ada.Asynchronous_Task_Control referring Target_Task
--      raise Tasking_Error.
--
-- APPLICABILITY CRITERIA:
--      This test is only applicable to implementations supporting:
--         the Real-Time Annex and
--         the package Asynchronous_Task_Control.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      09 Nov 95   SAIC    Added applicability criteria for ACVC 2.0.1
--      03 Feb 17   RLB     Added missing N/A error tag.
--
--!

with Report;
with ImpDef;   -- Implementation Defined Constants
with Ada.Task_Identification;
with Ada.Asynchronous_Task_Control;         -- N/A => ERROR. {1}

procedure CXDB001 is

   package AATC renames Ada.Asynchronous_Task_Control;
   package ATI  renames Ada.Task_Identification;

begin

   Report.Test ("CXDB001", "Asynchronous Task Control: Basic functions");

   declare -- encapsulate the test

      Target_Task_Delay : constant duration := ImpDef.Minimum_Task_Switch;

      -- Protected object for communication between the two tasks
      --
      protected TC is
         function  Get_ID return ATI.Task_Id;
         procedure Put_ID (I: ATI.Task_Id);

         function  Target_Task_Shut_Down return Boolean;
         procedure Set_Shut_Down;

         procedure Should_Be_Stopped;
         procedure Target_Task_Is_Running;
         function  Has_Run return Boolean;
         entry     Wait_Till_Has_Run;
      private
         Run_Flag : Boolean := false;
         Shut_Down_Flag : Boolean := false;
         Value   : integer := 0;    -- Computation is done here
         Hold_ID : ATI.Task_Id;     -- target task's ID
      end TC;

      protected body TC is

         function Get_ID return ATI.Task_Id is
         begin
            return Hold_Id;
         end Get_ID;

         procedure Put_ID (I: ATI.Task_Id) is
         begin
            Hold_ID := I;
         end Put_ID;

         function Target_Task_Shut_Down return Boolean is
         begin
            return Shut_Down_Flag;
         end Target_Task_Shut_Down;

         procedure Set_Shut_Down is
         begin
            Shut_Down_Flag := true;
         end;


         procedure Should_Be_Stopped is
         begin
            Run_Flag := false;
         end Should_Be_Stopped;

         procedure Target_Task_Is_Running is
         begin
            Run_Flag := true;
         end Target_Task_Is_Running;

         -- Has_Run returns true if Target_Task_Is_Running has been called
         -- at least once since the previous call to Should_Be_Stopped
         --
         function Has_Run return Boolean is
         begin
            return Run_Flag;
         end Has_Run;

         entry Wait_Till_Has_Run when Has_Run is
                                      -- Starts out false
         begin
            null;
         end Wait_Till_Has_Run ;

      end TC;


      --===============================

      task Target_Task;
      task Driver is
         entry Start;
      end Driver;

      --===============================

      task body Target_Task is
         This_Task_Id : ATI.Task_Id := ATI.Current_Task;
      begin

         TC.Put_ID ( This_Task_ID );  -- communicate ID to Driver

         while not TC.Target_Task_Shut_Down loop
            -- During the time this task is not Held it will be indicating
            -- this fact in the PO
            TC.Target_Task_Is_Running;
            -- Allow the Driver task to get control
            delay Target_Task_Delay;
         end loop;

      exception
         when others =>
         Report.Failed ("Unexpected Exception in Target_Task");
      end Target_Task;

      --===============================

      -- This task will Hold and Continue Task Target_Task

      task body Driver is

         Target_ID     : ATI.Task_Id;
         Current_Value : integer := 0;
         Last_Value    : integer := 0;

      begin

         -- Wait until we know that Target_Task is known to be active
         TC.Wait_Till_Has_Run;

         -- Get the target task's ID
         Target_ID := TC.Get_ID;

         -- Target_Task is running.  It is NOT being held. Check that Is_Held
         -- returns the proper value
         if AATC.Is_Held ( Target_ID ) then
            Report.Failed ("Unheld task is shown as Held");
         end if;

         --==============================

         -- Now hold the task and verify that it is not executing

         AATC.Hold ( Target_ID );

         TC.Should_Be_Stopped;

         -- Target_Task is Held, check the function
         if not AATC.Is_Held ( Target_ID ) then
            Report.Failed ("Held task is shown as not held");
         end if;

         -- Check the idempotency of the Hold operation
         declare
         begin
            AATC.Hold (Target_ID); -- should be no-op
            if not AATC.Is_Held ( Target_ID ) then
               Report.Failed ("Holding a Held task not a no-op");
            end if;
         exception
            when others =>
                Report.Failed ("Holding a Held task raises exception");
         end; -- declare

         -- Delay long enough for the Held task to indicate that
         -- it is (erroneously) still running
         delay (5 * Target_Task_Delay);

         if TC.Has_Run then
            Report.Failed ("Target_Task is not Held");
         else
            -- Now allow the Target_Task to continue
            AATC.Continue ( Target_ID );

            if AATC.Is_Held ( Target_ID ) then
               Report.Failed ("Continued task is shown as held");
            end if;

            -- Now delay long enough for Target_Task to run and to indicate
            -- that it is doing so
            delay (5 * Target_Task_Delay);
            if not TC.Has_Run then
               Report.Failed ("Target_Task is not running ");
            end if;

         end if;

         ------------------------------------------------------------

         -- Now check the effects of these calls applied against a
         -- terminated task

         TC.Set_Shut_Down;    -- Allow Target_Task to terminate
         --
         while not Target_Task'Terminated loop
            delay ImpDef.Minimum_Task_Switch;
         end loop;

         declare   -- check Hold
         begin
            AATC.Hold ( Target_ID );
            Report.Failed ("Hold Terminated Task. Tasking_Error not raised");
         exception
            when Tasking_Error => null;
            when others =>
                  Report.Failed ("Hold Terminated Task. Unexpected Exception");
         end;  -- Hold

         declare   -- check Continue
         begin
            AATC.Continue ( Target_ID );
            Report.Failed
                    ("Continue Terminated Task. Tasking_Error not raised");
         exception
            when Tasking_Error => null;
            when others =>
                Report.Failed
                    ("Continue Terminated Task. Unexpected Exception");
         end;  -- Continue

         declare   -- check Is_Held
         begin
            if AATC.Is_Held ( Target_ID ) then
               null;
            end if;
            Report.Failed
                    ("Is_Held Terminated Task. Tasking_Error not raised");
         exception
            when Tasking_Error => null;
            when others =>
                  Report.Failed
                    ("Is_Held Terminated Task. Unexpected Exception");
         end;  -- Is_Held

      exception
         when others =>
                 Report.Failed ("Unexpected Exception in Driver");
      end Driver;

   begin  -- declare

      null;

   end;   -- declare

   Report.Result;

end CXDB001;
