-- CXD2002.A
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
--      and a task executes a delay statement that does not result in 
--      blocking, it is added to the tail of the ready queue of its active
--      priority.
--
-- TEST DESCRIPTION:  
--      
--      Driver task is high priority.
--      Delay_Task and type Sub_Task are medium priority
--      Low_Priority_Task is low priority
-- 
--      All the tasks get spawned (three of type Sub_Task)  and each waits at a
--      rendezvous with Driver.   Driver makes the rendezvous with each.  The
--      rendezvous is completed at the priority of Driver and each task then
--      gets placed on the ready queue of its priority (as they are lower than
--      Driver).  Careful handshaking ensures that the order on the medium
--      queue is Delay_Task, then all the Sub_Tasks.  
--
--      Delay_Task should run first.  It is designed to rendezvous with Driver
--      do a delay 0.0 then Register.  After the rendezvous it should delay, 
--      get put on the back of the queue then Register when it reaches the 
--      front of the queue.   Sub_Task performs the rendezvous and then all
--      it does is Register; Low_Priority_Task does the same.  
--      
--      The order of Registration must be:  All the Sub_Tasks, Delay_Task,
--      Low_Priority_Task.
--      We repeat the whole process with delay_until a time in the past.
--      
-- APPLICABILITY CRITERIA:  
--      This test is not applicable to multiprocessors
--      as the low priority tasks could be started in parallel
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which are not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--     27 JUL 94   SAIC    Initial version
--     09 APR 95   SAIC    Redesign for 2.1
--     21 FEB 96   SAIC    New ImpDef Structure.
--                         Made names in description match code.
--                         Condensed range of priorities needed. 
--
--!

-----------------------  Configuration Pragmas --------------------

pragma Locking_Policy (Ceiling_Locking);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);


-------------------  End of Configuration Pragmas --------------------

with ImpDef;
with ImpDef.Annex_D;
use type ImpDef.Annex_D.Processor_Type;

with Report;
with Ada.Calendar;

with System;

procedure CXD2002 is

   Verbose     : constant Boolean := False;
   Priority_1Q : constant System.Priority := System.Default_Priority - 2;
   Priority_2Q : constant System.Priority := System.Default_Priority - 1;
   Priority_Hi : constant System.Priority := System.Priority'Last;

   Failed_1 : Boolean := false;
   Failed_2 : Boolean := false;
   Failed_3 : Boolean := false;

   Time_in_the_Past : Ada.Calendar.Time := Ada.Calendar.Time_of (1901,1,1);


begin


   Report.Test ("CXD2002", "Default Task Dispatching - a non-blocking delay " &
                           "puts the task at the tail " &
                           "of the ready queue for that priority");


   -- Some implementations will boot with the time set to 1901/1/1/0.0
   -- This delay is such that the implementation guarantees that a 
   -- subsequent call to Ada.Calendar.Time_Of(1901,1,1) will yield a 
   -- time that has already passed 
   --
   delay ImpDef.Delay_For_Time_Past;

   if ImpDef.Annex_D.Processor /= Impdef.Annex_D.Uni_Processor then
      Report.Not_Applicable ("Multi-Processor Configuration");
   else
   declare  -- encapsulate the test

      type Task_Ident is (Delay_Task_Type, Sub_Task_Type, Low_Priority_Type);

      protected Register is 
         procedure Running_Order (ID : Task_Ident);
         procedure Reset;
         procedure Delay_Complete;
         function Test_Complete return Boolean;
      private
         Delay_Task_Has_Registered : Boolean := false;
         Delay_Task_is_Complete    : Boolean := false;
         Test_1_Complete_Flag      : Boolean := false;
         SubTask_Count             : Natural := 0;
      end Register;

      protected body Register is
         procedure Running_Order  (ID : Task_Ident) is
         begin
            if ID = Delay_Task_Type then
               Delay_Task_Has_Registered := true;
            elsif ID = Sub_Task_Type then 
               -- This is a Sub_Task
               -- All Sub_Task tasks should register before the Delay_Task 
               -- registers
               if Delay_Task_Has_Registered  then 
                  Failed_1 := true;
               end if;
               -- keep track of how many subtasks register
               SubTask_Count := SubTask_Count + 1;
            else 
               -- This is the low priority task.  Since it is on the lower
               -- priority queue it must execute after the Delay_Task 
               -- completes the first phase. 
               if not Delay_Task_is_Complete  then 
                  Failed_2 := true;
               end if;
            end if;
         end Running_Order;
         
         -- Reset for the second part of the test
         procedure Reset is 
         begin
            Delay_Task_Has_Registered := false;
            SubTask_Count := 0;
            Delay_Task_is_Complete := false;
         end Reset;
      
         procedure Delay_Complete is 
         begin
            Delay_Task_is_Complete := true;
            Test_1_Complete_Flag   := true;
            -- make sure the subtasks have run
            if SubTask_Count /= 3 then
              Failed_3 := True;
            end if;
         end Delay_Complete;

         function Test_Complete return Boolean is
         begin
            return Test_1_Complete_Flag;
         end Test_Complete;


      end Register;

      --==============================
      
      task Driver_Task is 
         Pragma Priority ( Priority_Hi );
      end  Driver_Task;

      task Delay_Task is
         Pragma Priority ( Priority_2Q );
         entry Start_Test_1;
         entry Start_Test_2;
      end Delay_Task;

      task type Sub_Task is
         Pragma Priority ( Priority_2Q );
         entry Start;
      end Sub_Task;

      task Low_Priority_Task is
         Pragma Priority ( Priority_1Q );
         entry Start;
      end Low_Priority_Task;



      task body Sub_Task is
         This_Ident : Task_Ident := Sub_Task_Type;
      begin
         
         -- Wait to be called by Driver_Task
         accept Start;

         -- Once this task reaches the head of the ready queue it registers
         -- the fact that it is running
         --
         Register.Running_Order(This_Ident);
         
      exception
         when others => 
               Report.Failed ("Unexpected Exception in Sub_Task");
      end Sub_Task;


      task body Low_Priority_Task is

         This_Ident : Task_Ident := Low_Priority_Type;

      begin

         -- Wait to be called by Driver_Task
         accept Start;

         -- Once this task reaches the head of the ready queue it registers
         -- the fact that it is running
         --
         Register.Running_Order(This_Ident);

      exception
         when others => 
               Report.Failed ("Unexpected Exception in Low_Priority_Task");
      end Low_Priority_Task;



      -- For TEST I
      --
      -- Create three Sub_Tasks which will each wait to be called by 
      -- Driver_Task
      --
      Task_B : Sub_task;
      Task_C : Sub_task;
      Task_D : Sub_task;

      -- For TEST II
      --
      -- Create another set of three Sub_Tasks which will each wait to
      -- be called by Driver_Task
      --
      Task_Array : array (1..3) of Sub_Task;



      task body Delay_Task is 
      
         This_Ident : Task_Ident := Delay_Task_Type;

      begin

         --========================================================

         --  TEST I: NON BLOCKING DELAY_RELATIVE

         -- Wait to be called by Driver_Task
         accept Start_Test_1;

         -- When this section is executed the task has reached the
         -- head of the ready queue for its priority
         --
         -- Execute a non-blocking delay statement.  This should put the 
         -- task on the tail of the ready queue behind the Sub_Tasks
         --
         delay 0.0;

         -- When the task reaches the head of the queue once again it is
         -- reactivated here.  Register this fact.
         --
         Register.Running_Order(This_Ident);

         -- Now indicate that this part is complete.  If all has gone well
         -- this means that everything on the medium priority ready queue
         -- is finished, the queue is empty and the lower priority queue
         -- can be serviced
         --
         Register.Delay_Complete;

         -- Wait for the Driver to start the second part of the test
         accept Start_Test_2;


         -- When this section is executed the task has again reached the
         -- head of the ready queue for its priority
         --
         -- Execute a non-blocking delay statement.  This should put the 
         -- task on the tail of the ready queue behind the Sub_Tasks
         --
         delay until Time_in_the_Past;

         -- When the task reaches the head of the queue once again it is
         -- reactivated here.  Register this fact.
         --
         Register.Running_Order(This_Ident);

         -- Now indicate that this part is complete.  If all has gone well
         -- this means that everything on the medium priority ready queue
         -- is finished, the queue is empty and the lower priority queue
         -- can be serviced
         --
         Register.Delay_Complete;

      exception
         when others => 
               Report.Failed ("Unexpected Exception in Delay_Task");
      end Delay_Task;

      --===================

      task body Driver_Task is 
      
      begin

         --========================================================

         --  TEST I: NON BLOCKING DELAY_RELATIVE

         if Verbose then
           Report.Comment ("relative delay test");
         end if;

         -- Call the Delay_Task.  After completion of the rendezvous it
         -- will be placed on the head of the ready queue
         --
         Delay_Task.Start_Test_1;

         -- Call each of the subtasks, this puts them on the ready queue
         -- 
         Task_B.Start;
         Task_C.Start;
         Task_D.Start;

         -- Call the low priority task.  This puts it on the ready queue
         -- for its priority
         -- 
         Low_Priority_Task.Start;

         -- Now delay this high priority task.  At this point, as there
         -- are no other high priority tasks to execute, the lower 
         -- priority ready queues will be serviced.  All the waiting tasks
         -- will get run
         --
         while not Register.Test_Complete loop
            delay ImpDef.Clear_Ready_Queue;
         end loop;


         --========================================================
         --  TEST II: NON BLOCKING DELAY_UNTIL

         if Verbose then
           Report.Comment ("delay until test");
         end if;

         -- Reset for the second part of the test
         Register.Reset;

         -- Call the second entry of the Delay_Task, this puts it on 
         -- the ready queue
         --
         Delay_Task.Start_Test_2;

         -- Call each of the subtasks, this puts them on the ready queue
         for i in 1..3 loop
            Task_Array (i).Start;
         end loop;

         -- Now allow the high priority task to terminate. At this point, 
         -- as there are no other high priority tasks to execute, the medium 
         -- priority ready queue will be serviced.  (There is no low priority
         -- task involved in the second part of the test)

      exception
         when others => 
               Report.Failed ("Unexpected Exception in Driver_Task");
      end Driver_Task;
      
   begin    -- encapsulation

      null;

   end;     -- encapsulation
   end if;  -- applicability


   if Failed_1 then
      Report.Failed ("Delay_Task executed before a Sub_Task");
   end if;

   if Failed_2 then
      Report.Failed ("Low priority task executed out of order");
   end if;

   if Failed_3 then
      Report.Failed ("Sub_Tasks did not run when expected");
   end if;

   Report.Result;

end CXD2002;
