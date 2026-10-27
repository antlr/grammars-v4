-- CXD2001.A
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
--      and the setting of the base priority of a task takes effect, the
--      task is added to the tail of the ready queue of its active priority.
--
-- TEST DESCRIPTION: 
--      Two tasks of low priority are activated by a task of medium priority.
--      While they are on the low priority ready queue the medium priority
--      task changes its own priority to be the same as the children.  It
--      should go to the tail of the low priority queue.  Each task registers
--      its subsequent action with a protected object which checks the order
--      of the registration.  If the medium priority task is not suspended
--      but continues it will register before the low priority tasks and 
--      the test will fail. 
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which are not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
-- APPLICABILITY CRITERIA:  
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
--      This test is not applicable to multiprocessors
--      as the low priority tasks could be started in parallel.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      02 Nov 95   SAIC    Fixed priority values and other problems
--                          for ACVC 2.0.1
--      21 Feb 96   SAIC    New ImpDef structure for 2.1
--      11 Oct 96   SAIC    Incorporated improvement suggested by reviewer.
--
--!

-----------------------  Configuration Pragmas --------------------

pragma Locking_Policy (Ceiling_Locking);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);

-------------------  End of Configuration Pragmas --------------------


with ImpDef.Annex_D;
use type ImpDef.Annex_D.Processor_Type;
with Report;

with System;
with Ada.Dynamic_Priorities;

procedure CXD2001 is

   Package ADP renames Ada.Dynamic_Priorities;


   Priority_1Q : constant System.Priority :=
                           (System.Default_Priority - 1);
   Priority_2Q : constant System.Priority :=
                           (System.Default_Priority);
   Priority_3Q : constant System.Priority :=
                           (System.Default_Priority + 1);
   
   -- The priority of the test driver is set higher than all other tasks
   Pragma Priority ( Priority_3Q );

   TC_Failed : Boolean := False; 

   Prime_Order : array (1..3) of Boolean := (False, False, False);
   Order_Cnt : Integer := 0;

begin

   Report.Test ("CXD2001", "Setting base priority moves task to the end " &
                           "of the ready queue of that priority");

   if ImpDef.Annex_D.Processor /= ImpDef.Annex_D.Uni_Processor then
      Report.Not_Applicable ("Multi-Processor Configuration");
   else

   declare  -- encapsulate the test

      type Task_Type is (Prime_Task_Type, Sub_Task_Type);

      protected Register is 
         entry E1 (Registering_Type : Task_Type);
      private 
         Prime_Task_Has_Registered : Boolean := false;
      end Register;

      protected body Register is
         -- tasks call this entry to register their running sequence
         entry E1  (Registering_Type : Task_Type) when true is
         begin
            Order_Cnt := Order_Cnt + 1;
            if Registering_Type = Prime_Task_Type then
               Prime_Order (Order_Cnt) := True;
               Prime_Task_Has_Registered := true;
            else
               -- This is a Sub_Task
               -- Both Sub_Task tasks should complete before Prime_Task 
               -- registers
               -- All Subtasks must run before the Prime task is
               -- reactivated
               if Prime_Task_Has_Registered  then 
                  -- Cannot call Report.Failed from within a PO.
                  TC_Failed := true;
               end if;
            end if;
         end E1;
      end Register;
      

      task Prime_Task is 

         -- Set the priority of this task high to ensure that it runs
         -- to the exclusion of the Sub_Task children
         Pragma Priority ( Priority_2Q );

         entry Dummy_Entry;

      end Prime_Task;


      -- The priority of the Sub_Task is the priority of the ready queue
      -- we are using for the test
      --
      task type Sub_Task is
         Pragma Priority ( Priority_1Q );
      end Sub_Task;

      task body Sub_Task is
         This_Task_Type : Task_Type := Sub_Task_Type;
      begin
         -- Once this task reaches the head of the ready queue it registers
         -- the fact that it is running
         --
         Register.E1(This_Task_Type);
         
      exception
         when others => Report.Failed ("Unexpected Exception in Sub_Task");
      end Sub_Task;


      task body Prime_Task is 
         
         This_Task_Type : Task_Type := Prime_Task_Type;

         Task_B : Sub_Task;
         Task_C : Sub_Task;

      begin

         -- At this point the two Sub_Tasks are on the Priority_1Q queue
         -- waiting to run but this task is running at Priority_2Q.
         -- Now lower the priority of this task to the same as the others on
         -- the queue.
         --
         ADP.Set_Priority (Priority_1Q);   -- in ADP the default 
                                           -- Task_Identification is to 
                                           -- "current_task"

         -- When the setting of the active priority (in this case the base
         -- priority) takes effect the task should be suspended and added
         -- to the tail of the queue.  This dummy accept statement is 
         -- an abort completion point; the change in the priority must be 
         -- effective no later than this.
         --
         select
             accept Dummy_Entry;
         else
             null;
         end select;

         -- When the task reaches the head of the queue it is reactivated 
         -- here.  Register this fact.
         --
         Register.E1(This_Task_Type);

      exception
         when others => Report.Failed ("Unexpected Exception in Prime_Task");
      end Prime_Task;

   begin    -- encapsulation
      null;   
   end;     -- encapsulation
   end if;  -- applicability check

   if TC_Failed then
      Report.Failed ("Prime_Task executed before a Sub_Task");
      for I in Prime_Order'Range loop
         Report.Comment ("order:" & Integer'Image (I) &
                         "  is prime: " & 
                         Boolean'Image (Prime_Order (I)));
      end loop;
   end if;

   Report.Result;

end CXD2001;
