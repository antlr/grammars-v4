-- CXD4001.A
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
--      Check that when Priority Queuing is in effect and the base priority 
--      of a task is set (changed), the priorities of any queued calls from
--      that task are updated and that the ordering is modified accordingly.  
--
-- TEST DESCRIPTION:
--      Three tasks are generated at the default priority.  These tasks 
--      each call an entry in the distributor.  Handshaking between
--      the driver (Main), the tasks and the distributor ensures that
--      the tasks arrive in the queue in order.  Once all tasks are
--      queued the base priority of the third task is raised.  Further
--      handshaking allows the Distributor to now service the queue
--      and the order of the items being taken from the queue is
--      checked.  The call from the third task should be the one first
--      handled.
--
-- SPECIAL REQUIREMENTS:
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      09 Nov 95   SAIC    Provided time for priority change to
--                          take place.  ACVC 2.0.1
--
--!

------------------------ Configuration Pragmas --------------------------

pragma Queuing_Policy (Priority_Queuing);

------------------- End of Configuration Pragmas --------------------------


with System;
with Report;
with ImpDef;
with Ada.Task_Identification;
with Ada.Dynamic_Priorities;

procedure CXD4001 is


   package ATI renames Ada.Task_Identification;
   package ADP renames Ada.Dynamic_Priorities;

begin

   Report.Test ("CXD4001", "Reordering entry queues on Priority change");

   declare  -- encapsulate the test

      -- This is a priority that is higher than the default priority
      Priority_3Q : constant System.Priority :=
                        (System.Default_Priority + 5);
     
      -- Current length of the Distributor's Queue
      Current : integer; 
      
      -- Message Numbers for the test messages
      Message_1_Numb : integer := 100;
      Message_2_Numb : integer := 101;
      Message_3_Numb : integer := 102;

      -- Repository for the Task Identifications returned by the tasks
      -- at the Start entries
      Message_1_ID : ATI.Task_Id;
      Message_2_ID : ATI.Task_Id;
      Message_3_ID : ATI.Task_Id;
    
      task type Message_Task is
         entry  Start (Numb : in integer; Task_Id : out ATI.Task_Id);
      end Message_Task;
      type acc_Message_Task is access Message_Task;
      
      task Distributor is
         entry Input (Numb : integer);
         entry Go;
         entry Nb_waiting (Number : out Natural);
      end Distributor;

      task body Message_Task is
         This_Message_Number : integer;
      begin
         accept Start (Numb : in integer; Task_Id : out ATI.Task_Id) do

            -- Hold the "message" in this task.  For the test we are just 
            -- noting the Message_Number
            This_Message_Number := Numb;

            -- Get the system assigned Task_Id for this task and "return"
            -- it to the caller
            Task_Id := ATI.Current_Task;

         end Start;
         Distributor.Input(This_Message_Number);
      exception
         when others =>
                     Report.Failed ("Unexpected Exception in Message_Task");
      end Message_Task;

      task body Distributor is
      -- In an application the messages would be requeued to 
      -- other tasks downstream; for this test we just check 
      -- the arrival order
      begin
         loop
            -- This loop is used to wait for all the messages to arrive
            -- on the Input queue
            select
               accept Nb_waiting (Number : out Natural) do
                  Number := Input'Count;
               end Nb_waiting;
            or 
               accept Go;  
               exit;
            end select;
         end loop; 
        
         -- Now check the first message on the Input queue
         accept input (Numb : integer) do
           if Numb /= 102 then 
              Report.Failed ("Messages out of sequence");
           end if;
         end input;

         -- Purge the other messages
         for i in 1..2 loop
            accept input (Numb : integer);
         end loop;
         
      exception
         when others =>
                     Report.Failed ("Unexpected Exception in Distributor");
      end Distributor;

      -- Create three tasks, all at the default priority.  
      Message_1 : acc_Message_Task := new Message_Task;
      Message_2 : acc_Message_Task := new Message_Task;
      Message_3 : acc_Message_Task := new Message_Task;

   begin -- declare

      -- Start up the first Message carrier task.  For the test, just 
      -- present it with a message Number rather than a whole message.
      -- Save the Task Identification, which is returned at the end of the
      -- rendezvous, for later priority modification
      Message_1.Start (Message_1_Numb, Message_1_ID);
      
      -- Wait for the message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 1; 
         delay ImpDef.Minimum_Task_Switch;
      end loop;
      
      --===============

      -- Start the second 
      Message_2.Start (Message_2_Numb, Message_2_ID);

      -- Wait for the second message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 2;  
         delay ImpDef.Minimum_Task_Switch;
      end loop;

      --===============

      -- Start the third
      Message_3.Start (Message_3_Numb, Message_3_ID);

      -- Wait for the third message to arrive at the Distributor's queue
      loop
         Distributor.Nb_waiting (Current);
         exit when Current = 3;  
         delay ImpDef.Minimum_Task_Switch;
      end loop;

      --===============
      
      -- All messages are now waiting on the Distributor's queue.  
      -- Increase the priority of the third message. 
      ADP.Set_Priority ( Priority_3Q,  Message_3_ID );

      -- allow time for the priority change to take effect
      delay ImpDef.Minimum_Task_Switch;

      -- The third message should now have been moved to the front of
      -- the queue.  Start processing the messages, the distributor will now
      -- check the ordering.
      Distributor.Go;
     
   end; -- declare
   
   Report.Result;

end CXD4001;
