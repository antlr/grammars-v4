-- CXD4006.A
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
--      Check that if Queuing_Policy is Priority_Queuing, the calls to
--      an entry are queued in an order consistent with the priority of the
--      calls and that if an entry is removed and then reinserted it is 
--      added behind any other calls with equal priority in that queue.
--
-- TEST DESCRIPTION:
--      Nine test message tasks are generated, three each of different base 
--      priorities.  Each is given a unique message number which identifies
--      it.  Each message task queues itself on the  Input queue of the
--      Distributor.  Handshaking ensures that the order  of arrival at the
--      Input queue is known.  Once all the messages have been queued we know
--      that they will be ordered first by priority and  then FIFO order
--      within that priority.  We then set the base priority of the  first
--      message within the midrange priority (message number 201) but we do
--      not alter that priority  but set it to the same value as its original
--      base priority. It should be pulled and reinserted in the queue after
--      the other two tasks of the same priority.  The distributor is then
--      allowed to  proceed to the Input rendezvous where it services the
--      calls and checks the proper sequence
--
-- APPLICABILITY CRITERIA:  
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
-- 
-- SPECIAL REQUIREMENTS:
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      04 Nov 95   SAIC    Fixed reported problems for ACVC 2.0.1
--
--!

------------------------ Configuration Pragmas --------------------------

pragma Queuing_Policy (Priority_Queuing);

------------------- End of Configuration Pragmas --------------------------

with Report;
with ImpDef;

with System;
with Ada.Dynamic_Priorities;
with Ada.Task_Identification;

procedure CXD4006 is


   package ADP renames Ada.Dynamic_Priorities;
   package ATI renames Ada.Task_Identification;

begin

   Report.Test ("CXD4006", "Priority_Queuing: reinserted item " &
                                 "is queued behind those of same priority");


   declare  -- encapsulate the test

      Priority_1Q : constant System.Priority := System.Priority'First + 5;
      Priority_2Q : constant System.Priority := System.Default_Priority;
      Priority_3Q : constant System.Priority := Priority_2Q + 5;
     
      -- This is used by Await_Arrival and is the currently expected 
      -- Distributor queue length.  
      --
      Awaited : integer := 1;

      -- Hold the Task_Id of the one message whose priority we are going
      -- to reset later in the test
      --
      Message_201_Task_Id : ATI.Task_Id;
      pragma volatile (Message_201_Task_Id);
      
      type Message_Number is range 0..100_000;  

      type Message_Type is
         record
            Priority : System.Priority;
            Number   : Message_Number;
         end record;


      Max_Messages : constant := 9;

      -- This array is used to control the order of generation of the 
      -- Message_Tasks.  Indexing through the array gives the 
      -- Priority assigned to the next message and the message number. 
      -- Note the LH digit of the message number indicates its priority
      -- and the RH digit the sequence (of generation) within that priority
      -- NOTE: the ordering in the array causes the messages to be
      --       generated in a pseudo random fashion.
      -- 
      Send_Order : constant array (1..Max_Messages) of Message_Type := 
         ( (Priority_1Q, 101),      -- 1Q first in sequence
           (Priority_2Q, 201),      -- 2Q first in sequence
           (Priority_3Q, 301),      -- 3Q first in sequence

           (Priority_2Q, 202),      -- 2Q second in sequence
           (Priority_1Q, 102),
           (Priority_1Q, 103),

           (Priority_3Q, 302),
           (Priority_3Q, 303),      -- 3Q third in sequence
           (Priority_2Q, 203) );

    
      task type Message_Task is
         entry  Start (Numb          : in  Message_Number;
                       Task_Priority : in  System.Priority; 
                       Task_Id       : out ATI.TAsk_Id);
      end Message_Task;
      type acc_Message_Task is access Message_Task;
      
      task Distributor is
         entry Input (Numb : Message_Number);
         entry Go;
         entry Nb_waiting (Number : out Natural);
      end Distributor;

      -- This provides the handshaking to control the arrival of the 
      -- messages at the Distributor's Input queue
      -- 
      procedure Await_Arrival is
         Current : integer;
      begin

         loop
            -- Wait for the next message to arrive 
            Distributor.Nb_waiting (Current);
            exit when Current = Awaited; 
            delay ImpDef.Minimum_Task_Switch;
         end loop;
         Awaited := Awaited + 1;
      end Await_Arrival;
      
      task Generate_Messages is
         entry Start_Test;
      end Generate_Messages;
   

      -- Simulate the arrival of messages at the Distributor
      --
      task body Generate_Messages is
         begin
            accept Start_Test do  
               for Index in 1..Max_Messages loop
                  declare
                     -- Create a task for the next message segment
                     Next_Message_Task : acc_Message_Task := new Message_Task;

                     Next_Number       : Message_Number;
                     Next_Priority     : System.Priority;
                     Next_Task_Id      : ATI.Task_Id;
                  begin
                     -- Get the details of the message from the 
                     -- Send_Order array
                     Next_Number   := Send_Order (Index).Number;
                     Next_Priority := Send_Order (Index).Priority;
                     -- Start the task and present it with the required
                     -- priority and Message_Number
                     Next_Message_Task.Start (Next_Number, 
                                              Next_Priority,
                                              Next_Task_Id);

                     -- Later, we will want to set the base priority of
                     -- one of the Message_Tasks.  This one is the one
                     -- who's Message_Number is 201.  "Special Case" this
                     -- one only to note the Task_Id.
                     --
                     if Next_Number = 201 then 
                        Message_201_Task_Id := Next_Task_Id;
                     end if;

                     -- The sequence of arrival at the Input queue is 
                     -- important to the test.  Wait for the call from
                     -- the task created by this cycle through the loop
                     -- to arrive before continuing
                     -- 
                     Await_Arrival;

                  end;
               end loop;
            end start_Test;
      exception
         when others => 
              Report.Failed ("Unexpected Exception in Generate_Messages");
      end Generate_Messages;

      -- One of these tasks is created for each message
      -- 
      task body Message_Task is
         This_Message_Number : Message_Number;
      begin
         accept Start (Numb          : in  Message_Number;
                       Task_Priority : in  System.Priority;
                       Task_Id       : out ATI.TAsk_Id) do

            -- Set the priority of the task (uses the default
            --          Ada.Task_Identification.Current_Task)
            ADP.Set_Priority (Task_Priority);

            -- "Return" the current System defined Task_Id to the caller
            Task_Id := ATI.Current_Task;

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

         -- We expect the order processed to be the order of arrival
         -- within priority EXCEPT for 201 which should come at the end
         -- of its priority group because it was reinserted
         Expected_Order : constant array (1..Max_Messages) of Message_Number 
                           := ( 301, 302, 303,
                                202, 203, 201,
                                101, 102, 103 ); 
      begin
         loop
            select
               -- Used by the Await_Arrival to ensure pre-defined 
               -- arrival order
               -- 
               accept Nb_waiting (Number : out Natural) do
                  Number := Input'Count;
               end Nb_waiting;
            or 
               accept Go;  -- All messages are on the Input queue
               exit;
            end select;
         end loop;
   

         for Next_Expected in 1..Max_Messages loop

            accept Input (Numb : Message_Number) do 
               -- Process the input messages.  For this test just check
               -- the expected order
               -- 
               if Numb /= Expected_Order(Next_Expected) then
                  Report.Failed ("Messages out of sequence" &
                       " position:" & Integer'Image (Next_Expected) &
                       " expected:" & 
                       Message_Number'Image (Expected_Order(Next_Expected)) &
                       " actual:" &
                       Message_Number'Image (Numb));
               end if;
            end Input;

         end loop;
      exception
         when others =>
                     Report.Failed ("Unexpected Exception in Distributor");
      end Distributor;



   begin -- declare

      -- Generate the messages which queue themselves in the Input
      -- queue of the Distributor
      Generate_Messages.Start_Test;

      
      -- All messages are now waiting on the Distributor's queue.  
      -- Set the base priority of the chosen task.  This is actually 
      -- the same base priority as the task had originally so it should
      -- just be moved behind the others on the queue with the identical
      -- priority
      --
      ADP.Set_Priority (Priority_2Q, Message_201_Task_Id);

      -- allow plenty of time for the Set_Priority to take effect
      delay ImpDef.Clear_Ready_Queue;

      -- Allow the Distributor to proceed to the rendezvous with the 
      -- callers - the Distributor will now check the ordering 
      --
      Distributor.Go;
     
   end; -- declare
   
   Report.Result;

end CXD4006;
