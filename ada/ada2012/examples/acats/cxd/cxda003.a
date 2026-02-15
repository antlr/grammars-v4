-- CXDA003.A
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
--      Check that Set_False and Set_True can be called during a protected
--      operation that has its ceiling priority in the Interrupt_Priority
--      range.
--
-- TEST DESCRIPTION: 
--      A task is blocked waiting on a suspension object to become true.
--      The task is unblocked by 
--        1. the main procedure while executing in a
--           protected operation that has its ceiling priority set to 
--           Interrupt_Priority'First 
--        2. a task executing at interrupt priority.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which are not
--      part of any Compilation Unit;  the method employed is implementation
--      defined.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Real-Time Systems Annex.
--
--
-- CHANGE HISTORY:
--      06 Sep 95   SAIC    Initial Release for 2.1
--      20 Feb 96   SAIC    Incorporated Reviewer Comments
--
--!

--------------------------- Configuration Pragmas ---------------------

pragma Locking_Policy (Ceiling_Locking);

---------------------- End of Configuration Pragmas -------------------

with Ada.Synchronous_Task_Control;
with System;
with Report;
with ImpDef;
procedure CXDA003 is
   Priority_Int : constant System.Interrupt_Priority := 
                                             System.Interrupt_Priority'First; 
   Verbose : constant Boolean := False;
begin

   Report.Test ("CXDA003", "Check that a suspension_object can be changed" &
                           " while executing at an interrupt priority");
   
   declare -- encapsulate the test
      SO : Ada.Synchronous_Task_Control.Suspension_Object;
      In_Suspension : Boolean := False;
      pragma Volatile (In_Suspension);

      Did_Suspend, Did_Resume : Boolean := False;
      Unexpected_Exception : Boolean := False;

      protected Releaser is
         procedure Release_It;
         pragma Interrupt_Priority;
      end Releaser;

      protected body Releaser is
         procedure Release_It is
         begin
            Ada.Synchronous_Task_Control.Set_True (SO);
         end Release_It;
      end Releaser;

      task Does_Suspension is
         entry Do_It_Now;
      end Does_Suspension;

      task Task_of_Int_Priority is  
         pragma interrupt_priority ( Priority_Int );
         entry Do_Test (Did_Suspend, Did_Resume : out Boolean);
      end Task_of_Int_Priority;  


      -- These tasks call a protected object whose ceiling should be
      -- higher than the task's priority
      --   

      task body Does_Suspension is 
      begin
         loop
            select
                accept Do_It_Now;
            or
                terminate;
            end select;
            -- time to wait
            In_Suspension := True;
            Ada.Synchronous_Task_Control.Suspend_Until_True (SO);
            In_Suspension := False;
         end loop;
      exception
         when others =>
               Report.Failed ("Unexpected Exception in Does_Suspension");
      end Does_Suspension; 

       
      task body Task_of_Int_Priority is 
      begin
         accept Do_Test (Did_Suspend, Did_Resume : out Boolean) do
            Ada.Synchronous_Task_Control.Set_False (SO);
            -- let Does_Suspension block itself on SO
            Does_Suspension.Do_It_Now;
            delay ImpDef.Clear_Ready_Queue;
            Did_Suspend := In_Suspension;

            -- wake up Does_Suspension
            Ada.Synchronous_Task_Control.Set_True (SO);
            delay ImpDef.Clear_Ready_Queue;
            Did_Resume := not In_Suspension;
         end Do_Test;
      exception
         when others => Unexpected_Exception := True;
      end Task_of_Int_Priority; 


   begin
      begin
         Task_of_Int_Priority.Do_Test (Did_Suspend, Did_Resume);
      exception
         when others =>  Report.Failed ("exception propagated to main");
      end;

      if Did_Suspend then
         if Verbose then
            Report.Comment ("Check 1 - task is suspended");
         end if;
      else
         Report.Failed ("Task did not suspend (1)");
      end if;

      if Did_Resume then
         if Verbose then
            Report.Comment ("task version passed");
         end if;
      else
         Report.Failed ("task did not resume after Set_True" &
                        " from Interrupt_Priority task");
      end if;     

      if Unexpected_Exception then
         Report.Failed ("unexpected exception occurred");
      end if;


      Ada.Synchronous_Task_Control.Set_False (SO);
      -- let Does_Suspension block itself
      Does_Suspension.Do_It_Now;
      delay ImpDef.Clear_Ready_Queue;
      if In_Suspension then
         if Verbose then
            Report.Comment ("Check 2 - task is suspended");
         end if;
      else
         Report.Failed ("Task did not suspend (2)");
      end if;

      -- wake up Does_Suspension 
      Releaser.Release_It;
      delay ImpDef.Clear_Ready_Queue;
      if In_Suspension then
         Report.Failed ("task did not resume after Set_True" &
                        " from protected object");
      else
         if Verbose then
            Report.Comment ("protected object version passed");
         end if;
      end if;     

   exception
      when others =>
         Report.Failed ("Unexpected Exception in main procedure");
   end;  -- encapsulation

   Report.Result;

end CXDA003;
