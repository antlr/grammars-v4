-- CXD3002.A
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
--      Check that when Locking_Policy is Ceiling_Locking and no pragma
--      Priority, Interrupt_Priority, Interrupt_Handler or Attach_Handler is
--      specified in a protected definition the  Ceiling Priority of the 
--      protected object is System.Priority'Last
--
-- TEST DESCRIPTION: 
--      Define a Protected Object which has no Priority Pragmas.  This should
--      be given a ceiling of Priority'Last.  Create three tasks each of which
--      call the P.O.  Give the first a priority lower than Priority'Last just
--      to check reasonable access; the call should be successful.  Give the
--      second a priority of Priority'Last.  Since this is the same as the
--      priority ceiling of the PO the call should be successful.  Give the
--      third task the priority which is precisely one higher than the
--      expected ceiling (in this case Interrupt'First).  The call should fail
--      the ceiling check and Program_Error should be raised.  Verification
--      that the ceiling check is being performed correctly is carried out
--      elsewhere in the suite)
--
-- APPLICABILITY CRITERIA:  
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
--      This test is not applicable to implementations that do not
--      support tasks running with an active priority in the 
--      Interrupt_Priority range.
-- 
-- SPECIAL REQUIREMENTS
--      The implementation must process a configuration pragma which is not
--      part of any Compilation Unit; the method employed is implementation
--      defined.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      03 Nov 95   SAIC    Fixed priority problems for ACVC 2.0.1
--
--!

-----------------------  Configuration Pragmas --------------------

pragma Locking_Policy (Ceiling_Locking);

-------------------  End of Configuration Pragmas --------------------

   
with System;
with Report;

procedure CXD3002 is

   Priority_1Q  : constant System.Priority := System.Priority'First + 5;
   Priority_Top : constant System.Priority := System.Priority'Last;
   Priority_Int : constant System.Interrupt_Priority := 
                                             System.Interrupt_Priority'First; 
   
   Unexpected_Exception_In_Task  : Boolean := False;
   Expected_Exception_Not_Raised : Boolean := False;

   type Task_Number is range 1..3;    -- three tasks

   -- This is an array of flags showing which tasks have successfully 
   -- called the protected procedure
   --
   Check_Called : array (1..Task_Number'Last) of Boolean := (others => false);


   protected Protected_Object is
   
      -- This protected object has no Pragma Priority
      -- thus its priority ceiling should be System.Priority'Last

      procedure For_Ceiling_Check (Numb : Task_Number);  
      function Verify_Calls return Boolean;

   end Protected_Object;

  
  protected body Protected_Object is

      procedure For_Ceiling_Check (Numb : Task_Number) is 
         
      begin
         -- In order to verify the check of Ceiling_Priority 
         -- we must ensure that this procedure actually gets called.  
         -- If calls to this procedure were optimized out then this 
         -- part of the test would become a no-op.  The Check_Called
         -- array is checked at the end preventing optimization.
         --
         Check_Called (Numb) := true;

      end For_Ceiling_Check;

      function Verify_Calls return Boolean is
      begin
         -- The first two tasks should have registered but the last one
         -- will not have as Program_Error was raised by the call
         --
         if Check_Called (1) and 
            Check_Called (2) and not
            Check_Called (3) then
               return true;
         else
               return false;
         end if;

      end Verify_Calls;
      
   end Protected_Object;

begin

   Report.Test ("CXD3002", "Ceiling_Locking: Default Priority Ceiling");
   
   declare -- encapsulate the test


      task Task_of_1Q_Priority is   
         pragma priority (Priority_1Q);
      end Task_of_1Q_Priority;   

      task Task_of_Top_Priority is  
         pragma priority ( Priority_Top );
      end Task_of_Top_Priority;  

      task Task_of_Int_Priority is  
         pragma Interrupt_Priority ( Priority_Int );
      end Task_of_Int_Priority;  


      -- This task calls a protected object whose ceiling is higher
      -- than the task
      --   
      task body Task_of_1Q_Priority is 

         Numb : Task_Number := 1;
         
      begin
         Protected_Object.For_Ceiling_Check ( Numb );  -- Should be O.K.
      exception
         when others =>
               Report.Failed ("Unexpected Exception in Task_of_1Q_Priority");
      end Task_of_1Q_Priority; 


      -- This task calls a protected object whose declared ceiling is
      -- the same as the task's declared priority (no defaults)
      --
      task body Task_of_Top_Priority is 

         Numb : Task_Number := 2;

      begin
         Protected_Object.For_Ceiling_Check ( Numb );  -- Should be O.K.
      exception
         when others =>
               Report.Failed ("Unexpected Exception in Task_of_Top_Priority");
      end Task_of_Top_Priority; 

       
      -- This task calls a protected procedure whose declared ceiling is
      -- lower than the task's declared priority (no defaults). The
      -- ceiling check should raise Program_Error.
      --
      task body Task_of_Int_Priority is 
         Numb : Task_Number := 3;
      begin
         Protected_Object.For_Ceiling_Check ( Numb );  -- not o.k.
         Expected_Exception_Not_Raised := True;
      exception
         when Program_Error =>
            null;  
         when others =>
            Unexpected_Exception_In_Task := True;
      end Task_of_Int_Priority; 


   begin
      null;
   end;  -- encapsulation
   
   if Expected_Exception_Not_Raised then
      Report.Failed ("Program_Error not raised in Protected_Object");
   end if;

   if Unexpected_Exception_In_Task  then
      Report.Failed ("Unexpected Exception in Task_of_Int_Priority");
   end if;

   -- Now check that the calls were actually made and not optimized away
   -- Note: only two calls are actually made, the third results in an 
   -- exception being raised
   --  
   if not Protected_Object.Verify_Calls then 
      Report.Failed ("For_Ceiling_Check not called correctly");
   end if;

   Report.Result;

end CXD3002;
