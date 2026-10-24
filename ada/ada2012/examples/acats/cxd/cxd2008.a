-- CXD2008.A
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
--      Check that if the Task_Dispatching_Policy is 
--      FIFO_Within_Priorities and a blocked task becomes ready
--      then it is added to the tail of the ready queue for its 
--      priority.
--
-- TEST DESCRIPTION:
--      This test creates a number of tasks with a priority that
--      is lower than the main task.  The tasks first block
--      on a suspension object and then on a protected object.
--      In each case the tasks are unblocked in a particular order.
--      Once unblocked the tasks register with a protected object
--      that records the order in which the tasks arrived.  This
--      order is checked to see that they arrived in the order they
--      were unblocked.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which 
--      are not part of any Compilation Unit;  the method employed
--      is implementation defined.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Real-Time Systems Annex.
--      This test is not applicable to multi-processor systems.
--
--
-- CHANGE HISTORY:
--      12 DEC 95   SAIC    Initial release for 2.1
--      22 FEB 96   SAIC    Incorporated Reviewer Comments
--                          New ImpDef.
--
--!

--------------------------- Configuration Pragmas ---------------------

pragma Locking_Policy (Ceiling_Locking);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);

---------------------- End of Configuration Pragmas -------------------


with System;
with Report;
with ImpDef.Annex_D;        use type ImpDef.Annex_D.Processor_Type;
with Ada.Task_Identification;
with Ada.Synchronous_Task_Control;
procedure CXD2008 is
  Verbose : constant Boolean := False;

  package TID renames Ada.Task_Identification;
  package STC renames Ada.Synchronous_Task_Control;

  Max_Tasks : constant := 10;

----------------------------------------------------------
  -- registration of task running order
  type Id_Set is array (1..Max_Tasks) of Integer;
  protected Register is
     procedure Running (Id : Integer);
     procedure Start;
     function Get_Order return Id_Set;
  private
     Count : Integer := 0;
     Arrival_Order : Id_Set;
  end Register;

  protected body Register is
     procedure Running (Id : Integer) is
     begin
       Count := Count + 1;
       Arrival_Order (Count) := Id;
     end Running;

     procedure Start is
     begin
       Arrival_Order := (1..Max_Tasks => -1);
       Count := 0;
     end Start;

     function Get_Order return Id_Set is
     begin
       return Arrival_Order;
     end Get_Order;
  end Register;
 
----------------------------------------------------------

  -- first way to block - Suspension Objects
  Stop : array (1..Max_Tasks) of STC.Suspension_Object;

  -- second way to block - protected object
  protected type Semaphore is
    entry Wait;
    procedure Signal;
  private
    Open : Boolean := False;
  end Semaphore;

  Semaphores : array (1..Max_Tasks) of Semaphore;

  protected body Semaphore is
    entry Wait when Open is
    begin  
      Open := False;
    end Wait;

    procedure Signal is
    begin
      Open := True;
    end Signal;
  end Semaphore;
----------------------------------------------------------

  task type Low_Task is
    pragma Priority (System.Default_Priority - 1);
    entry Take_A_Number (Id : Integer);
  end Low_Task;

  task body Low_Task is
     Me : Integer;
  begin
    accept Take_A_Number (Id : Integer) do
      Me := Id; 
    end Take_A_Number;

    -- suspension object test
    STC.Suspend_Until_True (Stop (Me));
    Register.Running (Me);

    -- protected object test
    Semaphores (Me).Wait;
    Register.Running (Me);
  
  exception
    when others =>
      Report.Failed ("exception in low task" & Integer'Image (Me));
  end Low_Task;
    
----------------------------------------------------------


  -- verify that the tasks ran in the expected order

  procedure Check_Order (Msg : String) is
    Actual   : constant Id_Set := Register.Get_Order;
    Expected : constant Id_Set := (2, 4, 6, 8, 10, 1, 3, 5, 7, 9); 
  begin
    -- report on the results
    if Actual = Expected then
      if Verbose then
        Report.Comment ("pass " & Msg);
      end if;
    else
      Report.Failed (Msg);
      Report.Comment ("Expected  Actual");
      for I in Actual'Range loop
        Report.Comment (Integer'Image (Expected(I)) & 
                        Integer'Image (Actual(I)));
      end loop;
    end if;
  end Check_Order; 

----------------------------------------------------------

  procedure Do_The_Test is
     Bunch_Of_Tasks : array (1..Max_Tasks) of Low_Task;

     -- The main task needs to block until all the tasks in the
     -- Bunch_Of_Tasks have had time to do their thing.
     -- Since the Bunch_Of_Tasks cannot inform the main task when
     -- they are done another mechanism is needed to insure that
     -- they are given sufficient time to accomplish their work.
     -- This is done by having a task of lower priority than the
     -- Bunch_Of_Tasks that notifies the main task when it gets 
     -- a chance to run.  If it gets to run then the Bunch_Of_Tasks
     -- must not be able to run so they must be done.
     Just_Waiting : Semaphore;
 
     task Lowest_Priority is
        pragma Priority (System.Default_Priority - 2);
        entry Wake_Me_Up;
     end Lowest_Priority;
     task body Lowest_Priority is
     begin
        loop
           select
              accept Wake_Me_Up;
              Just_Waiting.Signal;
           or
              terminate;
           end select;
        end loop;
     end Lowest_Priority;

  begin
     -- give the tasks their Id
     for I in Bunch_Of_Tasks'Range loop
       Bunch_Of_Tasks(I).Take_A_Number(I);
     end loop;

     -- the tasks are ready to block themselves so give
     -- them a chance to do it.
     -- The blocking here depends upon priorities.
     -- Once we tell Lowest_Priority task that we are 
     -- ready to schedule a wakeup, the Lowest_Priority 
     -- task will not get a chance to run again until
     -- the Bunch_Of_Tasks have had a chance to run
     -- and put themselves to sleep.
     Lowest_Priority.Wake_Me_Up;
     Just_Waiting.Wait;

     -- clear out the register
     Register.Start;

     -- unblock the tasks in the expected order
     STC.Set_True (Stop(2));
     STC.Set_True (Stop(4));
     STC.Set_True (Stop(6));
     STC.Set_True (Stop(8));
     STC.Set_True (Stop(10));
     STC.Set_True (Stop(1));
     STC.Set_True (Stop(3));
     STC.Set_True (Stop(5));
     STC.Set_True (Stop(7));
     STC.Set_True (Stop(9));

     -- now give the tasks time to register and block again
     Lowest_Priority.Wake_Me_Up;
     Just_Waiting.Wait;

     Check_Order ("unblocked with Synchronous_Task_Control");

     -- do it again with protected objects being used for the blocking

     Register.Start;
     -- unblock in expected order
     Semaphores (2).Signal;
     Semaphores (4).Signal;
     Semaphores (6).Signal;
     Semaphores (8).Signal;
     Semaphores (10).Signal;
     Semaphores (1).Signal;
     Semaphores (3).Signal;
     Semaphores (5).Signal;
     Semaphores (7).Signal;
     Semaphores (9).Signal;

     -- give the tasks time to register
     Lowest_Priority.Wake_Me_Up;
     Just_Waiting.Wait;
   
     -- check the order the tasks were put onto the ready queue
     Check_Order ("unblocked with protected object");
  end Do_The_Test;

----------------------------------------------------------

begin
  Report.Test ("CXD2008",
               "Check that when a blocked task becomes ready, it is" &
               " added to the tail of the ready queue for its priority");

  if ImpDef.Annex_D.Processor /= ImpDef.Annex_D.Uni_Processor then
    Report.Not_Applicable ("Multi-Processor configuration");
  else
    Do_The_Test;
  end if;
  
  Report.Result;
end CXD2008;
