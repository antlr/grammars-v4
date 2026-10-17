-- CXD4004.A
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
--      Check that changes to the active priority of the caller do not
--      affect the priority of a call after it is first queued when the
--      queuing policy is priority queuing.
--
-- TEST DESCRIPTION:
--      Three calls of equal priority are made to the Input queue of the 
--      Distributor.  Before the calls are serviced, the priority of
--      the task making the last call is raised and held high; while it is
--      high the Distributor services the queue and checks that the last call
--      is  not processed out of turn.   The task who's priority is modified
--      (Modified_Task) uses an Asynchronous Select to place a call on the
--      Input queue and, in parallel, use Ada.Dynamic_Priorities.Set_Priority
--      to raise its own priority.
--
-- APPLICABILITY CRITERIA:  
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
-- 
-- SPECIAL REQUIREMENTS
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      03 Nov 95   SAIC    Fixed priority problems and replaced priority
--                          inheritance design for ACVC 2.0.1
--
--!

-----------------------  Configuration Pragmas --------------------

pragma Queuing_Policy (Priority_Queuing);

-------------------  End of Configuration Pragmas --------------------


with System;
with Report;
with ImpDef;
with Ada.Dynamic_Priorities;

procedure CXD4004 is

begin

   Report.Test ("CXD4004", "Change in Active priority of a caller" &
                              " does not affect priority of a queued call");


   declare  -- encapsulate the test

      Priority_1Q : constant System.Priority := System.Priority'First + 5;
      Priority_3Q : constant System.Priority := System.Default_Priority + 5;
     
      Current : integer;   -- Current Distributor queue length

      type Message_Number is range 0..100_000; 

      Max_Messages : constant := 3; 

      -- Message Numbers for the test messages
      Message_1_Numb : Message_Number := 100;   -- Low priority
      Message_2_Numb : Message_Number := 101;   -- Low priority
      Message_3_Numb : Message_Number := 102;   -- High active priority


      -- Two instances of a task of this type place the first two 
      -- messages on the queue 
      task type Message_Task is
         pragma priority ( Priority_1Q );
         entry  Start    ( Numb : in Message_Number );
      end Message_Task;
      type acc_Message_Task is access Message_Task;
      
      -- This task places the third message on the queue then has its
      -- active priority raised
      task Modified_Task is
         pragma priority ( Priority_1Q );
         entry  Start    ( Numb : in Message_Number );
      end Modified_Task;

      -- This task checks the entries on its Input queue
      task Distributor is
         entry Input (Numb : Message_Number);
         entry Go;
         entry Nb_waiting (Number : out Natural);
      end Distributor;

      --=================
    
      protected Signal is
           -- coordination between main procedure and Modified_Task
         procedure Priority_Raised;
         entry Wait_For_Priority_Raised;

           -- a good place to block forever
         entry Block;
      private
         Has_Raised : Boolean := False;
      end Signal;

      protected body Signal is
         procedure Priority_Raised is
         begin
            Has_Raised := True;
         end Priority_Raised;

         entry Wait_For_Priority_Raised when Has_Raised is
         begin
            null;
         end Wait_For_Priority_Raised;

         entry Block when False is
         begin
            null;
         end Block;
      end Signal;

      --=================

      task body Message_Task is
         This_Message_Number : Message_Number;
      begin
         accept Start (Numb          : in Message_Number) do

            -- Hold the "message" in this task.  For the test we are just 
            -- noting the Message_Number
            This_Message_Number := Numb;

         end Start;
         
         -- Now queue self on the Distributor's Input queue
         Distributor.Input(This_Message_Number);
         
      exception
         when others =>
                     Report.Failed ("Unexpected Exception in Message_Task");
      end Message_Task;

      task body Modified_Task is
         This_Message_Number : Message_Number;
      begin
         accept Start (Numb          : in Message_Number) do

            -- Hold the "message" in this task.  For the test we are just 
            -- noting the Message_Number
            This_Message_Number := Numb;

         end Start;
         
         -- Now use an ATC to queue self on the Distributor's Input queue
         -- and, in parallel raise our priority
         --
         select
            Distributor.Input(This_Message_Number);
         then abort
            -- While the Call to Input is waiting to be serviced, raise the
            -- priority of this task.
            Ada.Dynamic_Priorities.Set_Priority (Priority_3Q);

            -- announce that the priority is now raised
            Signal.Priority_Raised;
               
               -- We block here waiting on the Distributor's input
               -- queue to be serviced and triggering the async
               -- select (thus aborting this part).
            Signal.Block;

         end select;
         
      exception
         when others =>
                     Report.Failed ("Unexpected Exception in Modified_Task");
      end Modified_Task;


      task body Distributor is

         Next_Expected  : natural := 0;
         -- We expect the order processed to be the order of arrival even
         -- though the active priority of the last caller is modified while
         -- it is on the queue
         Expected_Order : constant array (1..Max_Messages) of Message_Number 
                           := ( 100, 101, 102); 
      begin
         loop
            -- This is the handshaking loop used to ensure all the items
            -- are queued before processing the Input queue
            select
               accept Nb_waiting (Number : out Natural) do

                  Number := Input'Count;
               end Nb_waiting;
            or 
               accept Go; 
               exit;
            end select;
         end loop;
   
         loop 
            -- Clear the queue and verify the sequence being handled
            accept Input (Numb : Message_Number) do 
               Next_Expected := Next_Expected + 1;

               if Numb /= Expected_Order(Next_Expected) then
                  Report.Failed ("Messages out of sequence");
               end if;
            end Input;

            -- When all items have been processed, allow the distributor
            -- to terminate
            exit when Input'Count = 0;
         
         end loop;

      exception
         when others =>
                     Report.Failed ("Unexpected Exception in Distributor");
      end Distributor;


      -- Create two tasks. They are both at the Priority_1Q level
      --
      Message_1 : acc_Message_Task := new Message_Task;  -- Low Priority
      Message_2 : acc_Message_Task := new Message_Task;  -- Low Priority

   begin -- declare

      -- Start up the Message carrier tasks.  For the test, just 
      -- present them with a message Number rather than a whole message.
      Message_1.Start (Message_1_Numb);

      -- Wait for the first message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 1; 
         delay ImpDef.Minimum_Task_Switch;
      end loop;
      
      Message_2.Start (Message_2_Numb);
      
      -- Wait for the second message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 2; 
         delay ImpDef.Minimum_Task_Switch;
      end loop;
      
      -- There are now two calls waiting on the queue.  Start up the 
      -- Modified_Task which will place the third call on the 
      -- queue.  
      Modified_Task.Start (Message_3_Numb);

      -- Wait for the third message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 3;  
         delay ImpDef.Clear_Ready_Queue;
      end loop;

      
      -- All messages are now waiting on the Distributor's queue.  
      -- Wait for Modified_Task to raise its priority.
      Signal.Wait_For_Priority_Raised;

      -- The Modified_Task is now running with an ACTIVE priority of
      -- Priority_3Q.  Allow the distributor to service the queue
      -- and to check the sequence of entries
      Distributor.Go;

   end; -- declare,   wait here till all the tasks have terminated
   
   Report.Result;

end CXD4004;
