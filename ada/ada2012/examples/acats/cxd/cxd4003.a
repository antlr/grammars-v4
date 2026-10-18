-- CXD4003.A
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
--      Check that if Queuing_Policy FIFO_Queuing is specified for a 
--      partition the task entry queues are handled in FIFO order and 
--      that the priorities of the calling tasks have no effect.
--
-- TEST DESCRIPTION: 
--      Generate three tasks.  The test driver calls each task in succession 
--      presenting it with a unique identification (Message_Number) and a
--      priority level.  The tasks use calls to Ada.Dynamic_Priorities to 
--      set their individual base priorities to that requested by the driver.
--      Each task calls the Distributor task and is queued.  Handshaking 
--      between the driver and the Distributor ensures that the arrival order 
--      (by unique identification) is known.  The higher priority calls are 
--      queued last.  Once all the calls are queued the driver (by 
--      handshaking) allows the Distributor to proceed to the rendezvous 
--      where the tasks are queued.  The distributor verifies that the tasks 
--      are processed in FIFO rather than priority order.   
--
-- SPECIAL REQUIREMENTS
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      09 Nov 95   SAIC    Fixed priorities for ACVC 2.0.1
--
--!


-----------------------  Configuration Pragmas --------------------

pragma Queuing_Policy (FIFO_Queuing);

-------------------  End of Configuration Pragmas --------------------


with System;
with Report;
with ImpDef;
with Ada.Dynamic_Priorities;

procedure CXD4003 is

   package ADP renames Ada.Dynamic_Priorities;

begin

   Report.Test ("CXD4003", "Check that if Queuing_Policy is FIFO_queuing" &
               " the queues are handled FIFO and priorities have no effect");


   declare  -- encapsulate the test

      Priority_1Q : constant System.Priority :=
                        (System.Priority'First + 5);
      Priority_2Q : constant System.Priority :=
                        (System.Priority'First + System.Priority'Last)/2;
      Priority_3Q : constant System.Priority :=
                        (System.Default_Priority + 5);
     
      Current : integer;   -- Current Distributor queue length
      pragma volatile (Current);

      type Message_Number is range 0..100_000;   

      Max_Messages : constant := 3;

      -- Message Numbers for the test messages
      Message_1_Numb : Message_Number := 100;   -- Low priority
      Message_2_Numb : Message_Number := 101;   -- Medium priority
      Message_3_Numb : Message_Number := 102;   -- High priority

    
      task type Message_Task is
         entry  Start (Numb          : in Message_Number;
                       Task_Priority : in System.Priority); 
      end Message_Task;
      
      task Distributor is
         entry Input (Numb : Message_Number);
         entry Go;
         entry Nb_waiting (Number : out Natural);
      end Distributor;

      task body Message_Task is
         This_Message_Number : Message_Number;
      begin
         accept Start (Numb          : in Message_Number;
                       Task_Priority : in System.Priority)  do

            -- Set the priority of the task (uses the default
            --          Ada.Task_Identification.Current_Task)
            ADP.Set_Priority (Task_Priority);

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


      task body Distributor is

         Next_Expected  : natural := 0;
         -- We expect the order processed to be the order of arrival even
         -- though they are arriving in inverse priority order.  (e.g. the 
         -- last call is from the highest priority task)
         Expected_Order : constant array (1..Max_Messages) of Message_Number 
                           := ( 100, 101, 102); 
      begin
         loop
            select
               accept Nb_waiting (Number : out Natural) do
                  Number := Input'Count;
               end Nb_waiting;
            or 
               accept Go;
            exit;
            end select;
         end loop;
   
         -- In a real situation this would more likely be an endless loop
         -- with a terminate alternative in the select.  For this test
         -- arbitrarily limit it to the number expected.
         for i in 1..Max_Messages  loop 
            select

               accept Input (Numb : Message_Number) do 
                  Next_Expected := Next_Expected + 1;

                  if Numb /= Expected_Order(Next_Expected) then
                     Report.Failed ("Messages out of sequence");
                  end if;
               end Input;

            end select;
         
         end loop;
      exception
         when others =>
                     Report.Failed ("Unexpected Exception in Distributor");
      end Distributor;


      -- Create three tasks. They start out at the default priority but 
      -- initial calls from the test driver will set the priorities as 
      -- required by the test.  
      --
      Message_1 : Message_Task;  -- will be the Low Priority task
      Message_2 : Message_Task;  -- will be the Medium Priority task
      Message_3 : Message_Task;  -- will be the High Priority task

   begin -- declare

      -- Start up the first Message carrier task.  For the test, just 
      -- present it with a message Number rather than a whole message.
      -- Save the Task Identification, which is returned at the end of the
      -- rendezvous, for later priority modification
      -- Make the first caller a Low priority task
      Message_1.Start (Message_1_Numb, Priority_1Q);
      
      -- Wait for the message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 1; 
         delay ImpDef.Minimum_Task_Switch;   
      end loop;
      
      -- Start the second 
      -- Make the second caller a Medium priority task
      Message_2.Start (Message_2_Numb, Priority_2Q);

      -- Wait for the second message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 2;  
         delay ImpDef.Minimum_Task_Switch;   
      end loop;

      -- Start the third
      -- Make the third caller a High priority task
      Message_3.Start (Message_3_Numb, Priority_3Q);

      -- Wait for the third message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 3;  
         delay ImpDef.Minimum_Task_Switch;   
      end loop;
      
      -- All messages are now waiting on the Distributor's queue.  
      -- Allow the Distributor to proceed to the rendezvous with the 
      -- callers - the distributor will now check that the ordering 
      -- is FIFO
      --
      Distributor.Go;
     
   end; -- declare
   
   Report.Result;

end CXD4003;
