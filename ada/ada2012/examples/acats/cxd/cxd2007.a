-- CXD2007.A
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
--      Check that a new running task is selected whenever there
--      is a nonempty ready queue with a higher priority than the
--      priority of the running task.
--      Check that when a task is preempted it is added to the
--      head of the ready queue for its active priority.
--
-- TEST DESCRIPTION:
--      This test uses a variety of methods to make a high priority
--      task ready to run so that it will preempt a running lower
--      priority task.  We check that the preemption does occur and
--      that the preempted task is put at the head of the ready
--      queue for its priority level.  This last check is accomplished
--      by having another task at the same priority as the preempted
--      task and checking that it did not get a chance to run.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which
--      are not part of any Compilation Unit;  the method employed
--      is implementation defined.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Real-Time Systems Annex.
--      This test is not applicable to implementations that do not
--      support Asynchronous_Task_Control.
--      This test is not applicable to multi-processor systems.
--
--
-- CHANGE HISTORY:
--      20 SEP 95   SAIC    Initial version
--      26 OCT 95   SAIC    Incorporated change suggested by reviewers
--      21 FEB 96   SAIC    New ImpDef; Smaller range of priorities used.
--      03 Feb 17   RLB     Added missing N/A error tag.
--
--!

--------------------------- Configuration Pragmas ---------------------

pragma Locking_Policy (Ceiling_Locking);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);

---------------------- End of Configuration Pragmas -------------------


with System;
with Report;
with ImpDef;
with ImpDef.Annex_D;        use type ImpDef.Annex_D.Processor_Type;
with Ada.Task_Identification;
with Ada.Asynchronous_Task_Control;         -- N/A => ERROR. {1}
with Ada.Synchronous_Task_Control;
procedure CXD2007 is
  Verbose : constant Boolean := False;

  package TID renames Ada.Task_Identification;
  package ATC renames Ada.Asynchronous_Task_Control;
  package STC renames Ada.Synchronous_Task_Control;

  type Names is (Low_A, Low_B, High);
  type Task_Flags is array (Names) of Boolean;

  -- each task is responsible for filling in its own entry
  Task_IDs : array (Names) of TID.Task_ID;

  Check_Suspension : Boolean := False;
  pragma Atomic (Check_Suspension);

  Suspension : STC.Suspension_Object;
----------------------------------------------------------

  protected Registry is
    procedure Running (Me : Names);
    procedure Clear;
    function Did_It_Run (It : Names) return Boolean;
  private
    Has_Run : Task_Flags := (others => False);
  end Registry;

  protected body Registry is
    procedure Running (Me : Names) is
    begin
      Has_Run (Me) := True;
    end Running;

    procedure Clear is
    begin
      Has_Run := (others => False);
    end Clear;

    function Did_It_Run (It : Names) return Boolean is
    begin
      return Has_Run (It);
    end Did_It_Run;
  end Registry;

----------------------------------------------------------

  task Low_A_Task is
    pragma Priority (System.Default_Priority - 2);
    entry Do_Register;
  end Low_A_Task;

  task body Low_A_Task is
  begin
    accept Do_Register do
      Task_IDs (Low_A) := TID.Current_Task;
    end Do_Register;

    -- make sure we are at the end of the low priority queue
    delay 0.0;

    loop
      -- we should never get a chance to run.  Getting to the
      -- next statement implies something failed.
      Registry.Running (Low_A);

      -- now that we recorded the failing condition, go back to
      -- the end of the ready queue so that the next part of the
      -- test can run.
      delay 0.0;
    end loop;
  end Low_A_Task;

----------------------------------------------------------

  task Low_B_Task is
    pragma Priority (System.Default_Priority - 2);
    entry Do_Register;
  end Low_B_Task;

  task body Low_B_Task is
  begin
    accept Do_Register do
      Task_IDs (Low_B) := TID.Current_Task;
    end Do_Register;

    loop
      Registry.Running (Low_B);
    end loop;
  end Low_B_Task;

----------------------------------------------------------

  task Hi_Task is
    -- higher priority than Low_* tasks but lower than main
    pragma Priority (System.Default_Priority - 1);
    entry Do_Register;
  end Hi_Task;

  task body Hi_Task is
  begin
    accept Do_Register do
      Task_IDs (High) := TID.Current_Task;
    end Do_Register;

    loop
      Registry.Running (High);
      -- until we get ready to do the suspension test we don't
      -- want to call Suspend_Until_True.  That is because
      -- Suspend_Until_True is a potentially blocking operation
      -- and we don't want to do any potentially blocking operation
      -- until it is time to do so.
      if Check_Suspension then
        STC.Suspend_Until_True (Suspension);
      end if;
    end loop;
  end Hi_Task;

----------------------------------------------------------

  -- verify that the tasks that were expected to run have
  -- run and only those tasks ran.
  -- The results are cleared after being checked in preparation
  -- for the next test.
  procedure Check (Expected : Task_Flags; Msg : String) is
    Actual : Task_Flags;
  begin
    -- capture all results before doing any reporting
    for I in Task_Flags'Range loop
       Actual (I) := Registry.Did_It_Run (I);
    end loop;

    -- report on the results
    if Actual = Expected then
      if Verbose then
        Report.Comment ("pass " & Msg);
      end if;
    else
      Report.Failed (Msg);
      for I in Task_Flags'Range loop
        if Actual (I) /= Expected (I) then
          if Expected (I) then
            Report.Failed (Names'Image (I) &
                           " was expected to run but didn't");
          else
            Report.Failed (Names'Image (I) &
                           " was not expected to run but did");
          end if;
        end if;
      end loop;
    end if;

    -- prepare for the next test
    Registry.Clear;
  end Check;

----------------------------------------------------------

begin
  Report.Test ("CXD2007",
               "Check that when a high priority task is eligible to" &
               " run it is given the processor and that the preempted" &
               " task stays at the head of the ready queue for its " &
               " priority");

  if ImpDef.Annex_D.Processor /= ImpDef.Annex_D.Uni_Processor then
    Report.Not_Applicable ("Multi-Processor configuration");
    abort Low_A_Task, Low_B_Task, Hi_Task;   -- clean up
    Report.Result;
    return;
  end if;

  -- make sure each task records its identity
  Low_A_Task.Do_Register;
  Low_B_Task.Do_Register;
  Hi_Task.Do_Register;

  -- the tasks should not have proceeded past their accept statements
  Check ((False, False, False), "elaboration and rendezvous");

  -- simple delay expiration
  delay ImpDef.Clear_Ready_Queue;
  Check ((False, False, True), "delay expiration preemption");

  -- hold the high priority task - it should not have run since the
  -- results were cleared above.
  ATC.Hold (Task_IDs (High));
  Check ((False, False, False), "hold should not be a blocking operation");

  -- a delay at this point will allow Low_B to run
  delay ImpDef.Clear_Ready_Queue;
  Check ((False, True, False), "high priority task is held");

  -- Low_B was preempted by the above delay expiring.  Make sure
  -- that it continues to run when we delay again.
  delay ImpDef.Clear_Ready_Queue;
  Check ((False, True, False), "preempted task stays at head of queue");

  -- continuing the held task should not give anyone else a chance to run
  ATC.Continue (Task_IDs (High));
  Check ((False, False, False),
         "continue should not be a blocking operation");

  -- make sure the continued task now runs
  delay ImpDef.Clear_Ready_Queue;
  Check ((False, False, True), "continued task should run");

  -- now, have the high priority task suspend itself
  Check_Suspension := True;
  STC.Set_False (Suspension);
  -- depending upon where Hi_Task is in its loop, it may or may
  -- not record that it has run prior to suspending.  We will go
  -- ahead and mark it as having run so that we can know what to
  -- expect as a result.
  Registry.Running (High);
  delay ImpDef.Clear_Ready_Queue;
  Check ((False, True, True), "started suspension");

  -- wakeup the high priority task
  Check_Suspension := False;
  STC.Set_True (Suspension);
  delay ImpDef.Clear_Ready_Queue;
  Check ((False, False, True), "after set_true");

  -- cleanup
  abort Hi_Task, Low_A_Task, Low_B_Task;


  Report.Result;
end CXD2007;
