-- CXD2003.A
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
--      and and a task's priority is lowered due to the loss of inherited
--      priority it is added to the head of the ready queue for its 
--      priority
--
-- TEST DESCRIPTION:  
--      Three tasks are created for each of three priorities.  They are
--      activated and each becomes blocked awaiting rendezvous with the
--      Test Driver (Prime_Task).  The Prime_Task then calls each one of
--      the sub tasks in a predetermined order.  As each sub task completes
--      its rendezvous it should be placed on the head of the ready queue 
--      for its priority.  Since the Prime_Task has the highest priority,
--      the sub tasks will inherit the higher priority during the 
--      rendezvous and then lose this inherited priority at the end of
--      the rendezvous.  This will result in the sub task being put at
--      the head of the ready queue for its priority.
--  
--      The queues should be serviced in priority sequence and as each task
--      reaches the head of the queue it registers with a protected object
--      which checks against the expected sequence.   This check is by
--      means of an ID which was presented to the task at the time of the
--      rendezvous.
--  
--      The tasks are called in a pseudo random (but predefined) order to
--      ensure that there is no residual sequencing imposed by the initial
--      activation.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which are not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
-- APPLICABILITY CRITERIA: This test is not applicable to multiprocessors
--      as the low priority tasks could be started in parallel
--
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      08 Dec 95   SAIC    Addressed protest and fixed priority expressions
--                          for ACVC 2.0.1
--      21 Feb 96   SAIC    New ImpDef structure for 2.1.
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

with System;

procedure CXD2003 is

   Priority_1Q : constant System.Priority :=
                           (System.Default_Priority - 1);

   Priority_2Q : constant System.Priority :=
                           (System.Default_Priority);

   Priority_3Q : constant System.Priority :=
                           (System.Default_Priority + 1);

   Failed_1 : Boolean := false;
   Failed_2 : Boolean := false;


   type Task_Identity is range 1..9;

   Expected_Sequence : array (1..9) of Task_Identity 
                  := ( 8, 5, 2,     -- the priority_3Q tasks in temporal order
                       7, 3, 1,     -- the priority_2Q 
                       9, 6, 4);    -- the priority_1Q 
   Actual_Sequence : array (1..9) of Task_Identity;
                       

begin


   Report.Test ("CXD2003", "Default Task Dispatching - when a task " &
                           "drops in priority due to loss of inheritance "&
                           "it is added to the head " &
                           "of the ready queue for its priority");


   if ImpDef.Annex_D.Processor /= Impdef.Annex_D.Uni_Processor then
      Report.Not_Applicable ("Multi-Processor Configuration");
   else
   declare  -- encapsulate the test
      protected  Register is 
         entry Check_Order (ID : Task_Identity);
         entry Prime_Complete;
      private
         Next : integer := 0;
         Prime_Task_is_Complete    : Boolean := false;
      end Register;


      protected body Register is

         entry Check_Order  (ID : Task_Identity) when true is
         begin
            if  Prime_Task_is_Complete then  
               Next := Next+1;
               Actual_Sequence (Next) := ID;
               if ID /= Expected_Sequence(Next) then
                  Failed_1 := true;
               end if;
            else
                  Failed_2 := true;
            end if;
         end Check_Order;
   
         entry Prime_Complete when true is
         begin
            Prime_Task_is_Complete := true;
         end Prime_Complete;
         
      end Register;

      --==============================
      
      task Prime_Task is
         -- Higher priority than all other tasks
         Pragma Priority ( System.priority'Last );
      end Prime_Task;

      task type Priority_1Q_Task is 
         Pragma Priority ( Priority_1Q );
         entry Start(ID : Task_Identity);
      end Priority_1Q_Task;

      task type Priority_2Q_Task is 
         Pragma Priority ( Priority_2Q );
         entry Start(ID : Task_Identity);
      end Priority_2Q_Task;

      task type Priority_3Q_Task is 
         Pragma Priority ( Priority_3Q );
         entry Start(ID : Task_Identity);
      end Priority_3Q_Task;

      --==============================

      task body Priority_1Q_Task is
         This_ID : Task_Identity; 
      begin
         
         -- Wait to be called by Prime_Task
         accept Start(ID : Task_Identity) do
            This_ID := ID;
         end Start;

         -- Once this task reaches the head of the ready queue it registers
         -- the fact that it is running
         --
         Register.Check_Order(This_ID);
         
      exception
         when others => 
                   Report.Failed ("Unexpected exception in Priority_1Q_Task");
      end Priority_1Q_Task;
      
      --==================

      task body Priority_2Q_Task is
         This_ID : Task_Identity; 
      begin
         
         -- Wait to be called by Prime_Task
         accept Start(ID : Task_Identity) do
            This_ID := ID;
         end Start;

         -- Once this task reaches the head of the ready queue it registers
         -- the fact that it is running
         --
         Register.Check_Order(This_ID);
         
      exception
         when others => 
                   Report.Failed ("Unexpected exception in Priority_2Q_Task");
      end Priority_2Q_Task;
      
      --==================

      task body Priority_3Q_Task is
         This_ID : Task_Identity; 
      begin
         
         -- Wait to be called by Prime_Task
         accept Start(ID : Task_Identity) do
            This_ID := ID;
         end Start;

         -- Once this task reaches the head of the ready queue it registers
         -- the fact that it is running
         --
         Register.Check_Order(This_ID);
         
      exception
         when others => 
                   Report.Failed ("Unexpected exception in Priority_3Q_Task");
      end Priority_3Q_Task;

      --==============================

      -- Create three tasks of each priority.  They will each be activated
      -- and will be blocked waiting to be called by Prime_Task
      --
      Task_1Q_A, Task_1Q_B, Task_1Q_C : Priority_1Q_Task;

      Task_2Q_A, Task_2Q_B, Task_2Q_C : Priority_2Q_Task;

      Task_3Q_A, Task_3Q_B, Task_3Q_C : Priority_3Q_Task;


      --==============================

      task body Prime_Task is 
      
         Current_ID : Task_Identity := 1;

         -- Create a unique identifier for each task
         --
         function Next_ID return Task_Identity is
            ID : Task_Identity;
         begin
            ID := Current_ID;
            if Current_ID /= Task_Identity'last then
               Current_ID := Current_ID + 1;
            end if;
            return ID;
         end Next_ID;

      begin

         -- Ensure all the lower priority tasks have been started and
         -- are blocked waiting to be called
         -- 
         delay ImpDef.Clear_Ready_Queue;


         -- Call each of the subtasks, give each an identifier. Call them
         -- in a "random" order.  As each is unblocked it should be 
         -- placed at the end of its priority queue. 
         -- NOTE: we call them in a "random" order to ensure there are no
         -- residual queuing effects left over from the original blocking
         -- order 
         -- 
         Task_2Q_B.Start (Next_ID);    -- ID will be 1
         Task_3Q_C.Start (Next_ID);    -- ID will be 2
         Task_2Q_A.Start (Next_ID);    -- ID will be 3
         Task_1Q_C.Start (Next_ID);    -- ID will be 4
         Task_3Q_A.Start (Next_ID);    -- ID will be 5
         Task_1Q_B.Start (Next_ID);    -- ID will be 6
         Task_2Q_C.Start (Next_ID);    -- ID will be 7
         Task_3Q_B.Start (Next_ID);    -- ID will be 8
         Task_1Q_A.Start (Next_ID);    -- ID will be 9

         Register.Prime_Complete;


      exception
         when others =>
                Report.Failed ("Unexpected exception in Prime_Task");
      end Prime_Task;
      
   begin    -- encapsulation

      null;

   end;     -- encapsulation
   end if;  -- inapplicability check

   --
   if Failed_1 then
      Report.Failed ("Incorrect sequence of Sub_Tasks");

      Report.Comment ("Order Expected Actual");
      for I in Actual_Sequence'Range loop
         Report.Comment (Integer'Image (I) & 
                         Task_Identity'Image (Expected_Sequence (I)) &
                         Task_Identity'Image (Actual_Sequence(I)));
      end loop;
   end if;

   if Failed_2 then
      Report.Failed ("Prime_Task ran out of sequence");
   end if;

   Report.Result;

end CXD2003;
