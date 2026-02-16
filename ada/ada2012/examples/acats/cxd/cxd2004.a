-- CXD2004.A 
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
--      Check that when Task_Dispatching_Policy is FIFO_Within_Priorities 
--      and the active priority of a running task is lowered due to loss of 
--      its inherited priority and there is a ready task of the same priority 
--      that is not running, the running task continues to run.
--
-- TEST DESCRIPTION: 
--      A high priority Driver task calls two default priority Sub_Tasks
--      ensuring they get placed on the default priority ready queue in a
--      known order. The Driver task then terminates leaving the default
--      priority queue as the only active ready queue.  Sub_Task_A is the
--      first on the queue so it is restarted and calls a very high
--      priority Protected Object.  While it is running the protected
--      object its inherited priority is high.  As it completes the
--      protected object it loses this inherited priority.  But, as there
--      is only another task of the same priority ready to run it should
--      not get suspended but should continue to run; it immediately
--      registers the fact that it is running.  When Sub_Task_A
--      terminates, Sub_Task_B is at the head of the ready queue and is
--      restarted.  It registers this action.  The routine with which
--      they both register determines the order of registration.  If
--      Sub_Task_B registers before Sub_task_A it means that the latter
--      was suspended and placed on the tail of the queue when it lost
--      its inherited priority and the test fails.
--
-- APPLICABILITY CRITERIA: This test is not applicable to multiprocessors
--      as the Sub_Tasks could be run in parallel
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which are not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      09 Nov 95   SAIC    Fixed priorities for ACVC 2.0.1
--      21 Feb 96   SAIC    New ImpDef for 2.1
--      17 Feb 97   PWB.CTA Changed order of Start calls for A and B.
--      29 Jun 98   EDS     Inserted zero delay to ensure Sub_Task_B is at
--                          the tail of its ready queue.
--      21 Nov 98   RLB     Added missing Not_Applicable check for
--                          multi-processor implementations.
--!


-----------------------  Configuration Pragmas --------------------

pragma Locking_Policy (Ceiling_Locking);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);

-------------------  End of Configuration Pragmas --------------------


with ImpDef;
with ImpDef.Annex_D;
use type ImpDef.Annex_D.Processor_Type;
with Report;

with System;

procedure CXD2004 is

   -- A priority which is higher than the Default Priority
   Priority_3Q : constant System.Priority :=
                           (System.Default_Priority + 1);

   -- We cannot use Report.Failed in the PO - bounded error.  Initialize these
   -- to true in case Register.Running_Order does not get called.
   -- 
   Failed_1 : Boolean := true;
   Failed_2 : Boolean := true;

   -- This protected object is executed by the task whos active
   -- priority we wish to raise.
   -- We must ensure that that call to this entry is not optimized
   -- away as the test depends on this inheritance
   --
   protected Very_High_Priority_PO is
      pragma Priority (System.Priority'Last);
      entry E1 (In_Value : integer);
      function Get_Foil return integer;
   private 
         Optimizer_Foil : integer :=1;
   end Very_High_Priority_PO;

   protected body Very_High_Priority_PO is

      entry E1 (In_Value : integer) when true is
      begin
         Optimizer_Foil := Optimizer_Foil + Report.Ident_Int(In_Value);
      end E1;
      
      function Get_Foil return integer is
      begin
         return Optimizer_Foil;
      end Get_Foil;

   end Very_High_Priority_PO;

begin

   Report.Test ("CXD2004", "Default Task Dispatching.  Loss of " &
       "inherited priority of active task does not cause suspension if " &
       "no higher priority task is ready");

   if ImpDef.Annex_D.Processor /= ImpDef.Annex_D.Uni_Processor then
      Report.Not_Applicable ("Multi-Processor configuration");
      Report.Result;
      return;
   end if;

   declare  -- encapsulate the test

      type Task_Identity is (Task_A_ID, Task_B_ID);


      protected  Register is 
         procedure Running_Order (ID : Task_Identity);
      private
         Sub_Task_A_Has_Registered : Boolean := false;
         Sub_Task_B_Has_Registered : Boolean := false;
      end Register;


      -- The sequence of registration of the two Sub_Tasks is recorded here
      --
      protected body Register is

         procedure Running_Order  (ID : Task_Identity) is
         begin
            case ID is 
               when Task_A_ID => 
                  Sub_Task_A_Has_Registered := true;
                  if not Sub_Task_B_Has_Registered then
                     Failed_1 := false;
                  end if;
               when Task_B_ID => 
                  Sub_Task_B_Has_Registered := true;
                  if Sub_Task_A_Has_Registered then
                     Failed_2 := false;
                  end if;
            end case;
         end Running_Order;

      end Register;


      task Sub_Task_A is
         entry Start;
      end Sub_Task_A;

      task Sub_Task_B is
         entry Start;
      end Sub_Task_B;

      task body Sub_Task_A is
         -- This task has the default priority
         This_ID : Task_Identity := Task_A_ID;
         Dummy_Int : constant integer := 5;
      begin

         accept Start;
         --
         -- The Driver task has higher priority so, after the rendezvous is 
         -- complete this task is placed on the ready queue waiting to 
         -- resume.  From the nature of the test Sub_Task_B will also be
         -- on the queue behind this task at that time.
         
         -- Upon reactivation:
         -- During this call to the  PO the inherited priority will be raised.
         --
         Very_High_Priority_PO.E1 (Dummy_Int);

         -- Now that we are back from the PO our priority has reverted. 
         -- Register the running order. Even though there is another task 
         -- of the same priority on the ready queue this task should continue
         -- to run
         --
         Register.Running_Order (This_ID);
         
      exception
         when others => Report.Failed ("Unexpected exception in Sub_Task_A");
      end Sub_Task_A;

      task body Sub_Task_B is
         -- This task has the default priority
         This_ID : Task_Identity := Task_B_ID;
      begin

         delay 0.0;
         accept Start;
         -- After the rendezvous this task will be placed on the ready
         -- queue.  It will be behind Sub_Task_A.
         
         -- Upon reactivation we just register.  By this time Sub_Task_A 
         -- should have already registered UNLESS it was suspended.
         -- 
         Register.Running_Order (This_ID);
         
      exception
         when others => Report.Failed ("Unexpected exception in Sub_Task_B");
      end Sub_Task_B;

      task Driver_Task is
         -- This task runs at a higher priority than either of the Sub_Tasks
         pragma priority (Priority_3Q);
      end Driver_Task;
      task body Driver_Task is
      begin
         -- Call both sub tasks to ensure they are on the ready queue in the 
         -- proper order
         Sub_Task_B.Start;  -- When finished, B is preempted (by Driver)
         Sub_Task_A.Start;  -- When finished, A is preempted (by Driver)
                            -- so goes at the head of the queue.

         -- Allow this task to terminate - no higher priority tasks will then
         -- be running so the subtasks will be reactivated in order from the 
         -- default priority ready queue

      exception
         when others => Report.Failed ("Unexpected exception in Driver_Task");
      end Driver_Task;

   begin    -- encapsulation
      null; 
   end;  

   if Failed_1 then
      Report.Failed ("B registered before A");
   end if;

   if Failed_2 then
      Report.Failed ("A did not register before B");
   end if;

   -- Get the value of the optimizer foil and take action depending
   -- on the value - this increases difficulty of optimizing away 
   -- the original call to E1.  Note: if this check fails the test is 
   -- definitely failed but it is not necessarily a failure of the actual
   -- objective.
   -- 
   if Very_High_Priority_PO.Get_Foil = 1 then
      Report.Failed ("Optimizer_Foil value did not change");
   end if;

   Report.Result;

end CXD2004;
