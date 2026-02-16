-- CXD5001.A
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
--      Check that for Get_Priority, Tasking_Error is raised if the
--      specified task has terminated.  Check that for Get & Set Priority,
--      Program_Error is raised if the task has a null Task_Identification.  
--
-- TEST DESCRIPTION:
--      Target_Task is brought up and allowed to terminate; Get_Priority is
--      issued specifying Target_Task.  Get and Set priority calls are 
--      then issued with an uninitialized Task_Id as the parameter. 
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      09 Nov 95   SAIC    Fixed priorities for ACVC 2.0.1
--
--!

with Report;
with System;
with ImpDef;
with Ada.Task_Identification;
with Ada.Dynamic_Priorities;

procedure CXD5001 is
 
   package ATI renames Ada.Task_Identification;
   package ADP renames Ada.Dynamic_Priorities;

begin -- CXD5001

   declare  -- encapsulate the test

      Priority_1Q : constant System.Priority :=
                            (System.Priority'First + 5);

      Target_Task_Id    : ATI.Task_Id;
      Uninitialized_Id  : ATI.Task_Id;   -- Note: default initialized
                                         -- to Null_Task_ID
      Target_Task_Priority : System.Priority;  


      --======================

      task Target_Task is
         entry Get_Id(Task_Id : out ATI.Task_Id);
      end Target_Task;

      task body  Target_Task is
      begin
         accept Get_Id (Task_Id : out ATI.Task_Id)  do
            -- Get the system assigned Task_Id for this task and 
            -- "return" it to the caller
            Task_Id := ATI.Current_Task;
         end Get_Id;

         -- once the rendezvous is complete this task will terminate

      exception
         when others => 
               Report.Failed ("Unexpected Exception in Target_Task");
      end Target_Task;

      --======================


   begin  

      Report.Test ("CXD5001", "Dynamic Priorities.  " &
                                           "Terminated or Null tasks");

      Target_Task.Get_Id (Target_Task_Id);

      -- Ensure Target_Task is Terminated before starting the rest of
      -- the test
      --
      while not Target_Task'Terminated loop
         delay ImpDef.Minimum_Task_Switch;
      end loop;

      --==========================

      declare   -- Subtest 01

      -- We now invoke Get_Priority on a task we know is terminated
      -- Tasking_Error should be raised
      begin
         Report.Comment ("Subtest 01");
         Target_Task_Priority := ADP.Get_Priority (Target_Task_Id);
         Report.Failed ("Tasking_Error not raised in Subtest 01");
      exception
         when Tasking_Error => null;
         when others =>
                  Report.Failed ("Unexpected Exception in Subtest 01");
      end;

      --==========================

      declare   -- Subtest 02

      -- We now invoke Get_Priority using a Null_Task_Id
      -- Program_Error should be raised
      begin
         Report.Comment ("Subtest 02");
         Target_Task_Priority := ADP.Get_Priority (Uninitialized_Id);
         Report.Failed ("Program_Error not raised in Subtest 02");
      exception
         when Program_Error => null;
         when others =>
                  Report.Failed ("Unexpected Exception in Subtest 02");
      end;
      --==========================

      declare   -- Subtest 03

      -- We now invoke Set_Priority  using a Null_Task_Id
      -- Program_Error should be raised
      begin
         Report.Comment ("Subtest 03");
         ADP.Set_Priority (Priority_1Q, Uninitialized_Id);
         Report.Failed ("Program_Error not raised in Subtest 03");
      exception
         when Program_Error => null;
         when others =>
                  Report.Failed ("Unexpected Exception in Subtest 03");
      end;

      --==========================

   end;  -- encapsulation

   Report.Result;

end CXD5001;
