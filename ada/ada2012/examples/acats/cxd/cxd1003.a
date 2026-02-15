-- CXD1003.A
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
--      Check that during rendezvous, the task accepting the entry call
--      inherits the active priority of the caller.  Specifically, check 
--      when the caller has a higher priority than the receiver. 
-- 
-- TEST DESCRIPTION: 
-- 
--      The rules of inheritance are such that the rendezvous will be
--      performed at the maximum of all the inherited priorities.  The base
--      priority of the receiver is a source of inheritance and is low but
--      the caller is high so the rendezvous must be performed at the
--      priority of the caller.
--      
--      We use the fact that a call to a Protected object whose Ceiling
--      Priority is lower than the caller's will result in a Program Error
--      being raised.  We arrange to call an entry in a task which has a low
--      priority from a task with high priority.  We have a Protected Object
--      whose Ceiling is between the priority of the low and high priority
--      tasks.  We call the PO from the lower priority task to make sure that
--      the call can be made.  We then call the low priority task from the high
--      and in the rendezvous call the PO again.  This time, as the rendezvous
--      should inherit the High priority of the caller the Ceiling priority of
--      the PO will be violated raising program error.
--
--      Since implementations are permitted to round up ceilings to the highest
--      priority in the range we use System.Priority for the priorities of the
--      low priority task and the PO and we use Interrupt_Priority for the
--      calling task.
--
-- APPLICABILITY CRITERIA:  
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
--      This test is not applicable to implementations that do not
--      support tasks running with an active priority in the Interrupt_Priority
--      range.
-- 
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      02 Nov 95   SAIC    Fixed priority problems for ACVC 2.0.1
--
--!

Pragma Locking_Policy (Ceiling_Locking);
   
with System; 
with Report;

procedure CXD1003 is

   Priority_1Q  : constant System.Priority :=
                            (System.Priority'First + 3);
   Priority_Top : constant System.Priority := 
                            System.Priority'Last;
   Priority_Int : constant System.Interrupt_Priority :=
                            System.Interrupt_Priority'First;
   
   type Call_ID is range 1..2; 
   --
   -- This is an array of flags showing the calls to the
   -- the protected procedure.  
   Check_Called : array (1..Call_ID'Last) of Boolean := (others => false);

   -- test conditions detected during interrupt priority processing
   Expected_Exception_Not_Raised_1 : Boolean := False;
   Expected_Exception_Raised_1     : Boolean := False;
   Unexpected_Exception_Raised_1   : Boolean := False;
   Unexpected_Exception_Raised_2   : Boolean := False;

begin

   Report.Test ("CXD1003", "Priority: Rendezvous inherits" &
                                               " active priority of caller");
   
   declare -- encapsulate the test

      protected Protected_Object is
      
         -- Specify the Ceiling Priority for this Protected Object
         pragma priority (Priority_Top);

         procedure For_Ceiling_Check (ID : Call_ID);  

      end Protected_Object;

     
     protected body Protected_Object is

         procedure For_Ceiling_Check (ID : Call_ID) is 
         --
         begin
            -- In order to verify the check of Ceiling_Priority 
            -- we must ensure that this procedure actually gets called.  
            -- Since this accesses a global which is later checked the
            -- call will not be optimized away.
            --
            Check_Called (ID) := true;
            --
         end For_Ceiling_Check;
      
      end Protected_Object;

      --===========

      task Normal_Task is   
         pragma priority (Priority_1Q);
         entry Interrupt_Action;
      end Normal_Task;   

      task Interrupt_Task is  
         pragma Interrupt_Priority ( Priority_Int );
      end Interrupt_Task;  

      -- This task has the low priority.  It is called by the high priority
      -- task.  We check that the rendezvous is performed at the high
      -- priority
      --   
      task body Normal_Task is 

         ID1 : Call_ID := 1;
         ID2 : Call_ID := 2;
         
      begin
         declare  -- scope for good call
         begin 
            -- Call the PO.  The ceiling is higher than the active priority
            -- of this task so the call should be accepted
            --
            Protected_Object.For_Ceiling_Check ( ID1);  -- Should be O.K.
         exception 
            when others => 
               Report.Failed ("Exception raised in first call to PO");
         end;   -- scope for good call

         -- Now wait to be called by the Interrupt Task
         accept Interrupt_Action do
            -- The rendezvous should inherit the priority of the Interrupt
            -- Task.  The call to the PO will now be at a higher priority 
            -- than the ceiling so Program_Error should be raised
            --
            Protected_Object.For_Ceiling_Check ( ID2);  

            Expected_Exception_Not_Raised_1 := True;

         exception 
            when Program_Error => -- expected behavior
               Expected_Exception_Raised_1 := True;
            when others =>
               Unexpected_Exception_Raised_1 := True;
         end Interrupt_Action;

      exception
         when others =>
               Report.Failed ("Unexpected Exception in Normal_Task");
      end Normal_Task; 


      -- This task has a high priority.  When it calls the Interrupt_Action
      -- that rendezvous should inherit the high priority.
      --
      task body Interrupt_Task is 

      begin
         Normal_Task.Interrupt_Action;
      exception
         when others =>
               -- Program_Error should be raised in the rendezvous but it
               -- should be handled there and not propagated.
               Unexpected_Exception_Raised_2 := True;
      end Interrupt_Task; 


   begin
      null;
   end;  -- encapsulation

   --=====================

   -- Now check that the first call to the PO was made and that the second 
   -- was not
   if not Check_Called (1) then
      -- The legitimate call did not take place
      Report.Failed ("First call to For_Ceiling_Check not effective");
   end if;
   if Check_Called (2) then
      -- Ceiling check for the high priority call failed to prevent the call
      Report.Failed ("Second call For_Ceiling_Check not prevented");
   end if;


   if Expected_Exception_Not_Raised_1 then
      Report.Failed ("Expected exception not raised");
   end if;

   if not Expected_Exception_Raised_1     then
      Report.Failed ("Expected exception did not occur");
   end if;

   if Unexpected_Exception_Raised_1   then
      Report.Failed ("Unexpected exception raised (1)");
   end if;

   if Unexpected_Exception_Raised_2   then
      Report.Failed ("Unexpected exception raised (2)");
   end if;


   Report.Result;

end CXD1003;
