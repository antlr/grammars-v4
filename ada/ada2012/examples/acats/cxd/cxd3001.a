-- CXD3001.A
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
--      Check that Program_Error is raised if a task calls a protected
--      operation who's ceiling is lower than the task's active priority.
--      Check this for Function, Procedure and Entry.  Check that the 
--      exception is not raised if the ceiling is equal to or higher than
--      the priority of the calling task.
--
-- TEST DESCRIPTION:
--      Create a protected object with a mid-range priority and which 
--      contains  a procedure, a function and an entry.  Create two tasks,
--      one having  a priority lower than the PO and one having a priority 
--      equal to it; each of the tasks calls each of the subprograms and 
--      entry in the PO checking that Program_Error is NOT raised. 
--
--      Create three tasks with priority higher than the PO.  The tasks in 
--      turn  call one of the subprograms and the entry.  In each case check 
--      that Program_Error is raised.
--
-- APPLICABILITY CRITERIA:  
--      This test is only applicable to implementations supporting the
--      Real-Time Annex.
--      This test is not applicable to implementations that do not
--      support tasks running with an active priority in the Interrupt_Priority
--      range.
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


with Report;
with System;

procedure CXD3001 is                  

   -- Because implementations have permission to round up the ceiling
   -- priorities to the top of the current range we have to choose
   -- Interrupt priority and call a PO of ceiling Priority'Last to ensure
   -- we get a call to a PO with a lower ceiling.  For equality we 
   -- deliberately choose Priority'Last for both in case such rounding is
   -- in effect.
   
   Priority_LO : constant System.Priority := (System.Priority'First);
   Priority_MD : constant System.Priority := (System.Priority'Last);
   Priority_HI : constant System.Interrupt_Priority := 
                                      (System.Interrupt_Priority'First); 


   Unexpected_Exception_in_Proc : Boolean := False;
   Unexpected_Exception_in_Func : Boolean := False;
   Unexpected_Exception_in_Ent  : Boolean := False;
   Expected_Exception_Not_Raised_in_Proc : Boolean := False;
   Expected_Exception_Not_Raised_in_Func : Boolean := False;
   Expected_Exception_Not_Raised_in_Ent  : Boolean := False;
   Unexpected_Code_Reached : Boolean := False;
begin


   Report.Test ("CXD3001", "Locking_Policy: Ceiling Locking. " & 
                                 "Calling protected operations");
   
   declare -- encapsulate the test

      protected Protected_Object is
      
         pragma priority (Priority_MD);

         procedure P_procedure;   
         function  P_function return natural;
         entry  P_entry; 

      private
         -- In order to verify the check of Ceiling_Priority 
         -- we must ensure that the calls to the subprograms and entry 
         -- actually get executed and not optimized away.  Each one accesses 
         -- this variable which gets checked at the end of the test.
         -- 
         Number_of_Calls : natural := 0;

      end Protected_Object;

      protected body Protected_Object is

         procedure P_procedure is
         begin
            Number_of_Calls :=   Number_of_Calls + 1;
         end P_procedure;
      
         function  P_function return natural is
         begin
            return Number_of_Calls;
         end P_function;
      
         entry  P_entry when true is
         begin
            Number_of_Calls :=   Number_of_Calls + 1;
         end P_entry;
      end Protected_Object;

      --====================================================


      -- This task calls a protected object whose ceiling is higher
      -- than the task
      --   
      task Task_LO is  
         pragma priority (Priority_LO);
      end Task_LO;  
      --
      task body Task_LO is 
         
         Func_Return : natural;

      begin

         -- Call each of the items in the PO. None should raise an exception
         --
         Protected_Object.P_procedure;                   -- Should be O.K.
         Protected_Object.P_entry;                       -- Should be O.K.
         Func_Return := Protected_Object.P_function;     -- Should be O.K.

         -- Now execute a dummy routine with an external effect to make use
         -- of the result returned by the function.  This ensures that
         -- nothing will be optimized away
         -- 
         if Func_Return = natural'Last then
            Unexpected_Code_Reached := True;
         end if;

      exception
         when others =>
               Report.Failed ("Unexpected Exception in Task_LO");
      end Task_LO; 

      --=====================

      -- This task calls a protected object whose declared ceiling is
      -- the same as the tasks declared priority (no defaults)
      --
      task Task_MD is  
         pragma priority (Priority_MD);
      end Task_MD;
      --
      task body Task_MD is 

         Func_Return : natural;

      begin

         -- Call each of the items in the PO. None should raise an exception
         --
         Protected_Object.P_procedure;                   -- Should be O.K.
         Protected_Object.P_entry;                       -- Should be O.K.
         Func_Return := Protected_Object.P_function;     -- Should be O.K.

         -- Now execute a dummy routine with an external effect to make use
         -- of the result returned by the function.  This ensures that
         -- nothing will be optimized away
         -- 
         if Func_Return = natural'Last then
            Unexpected_Code_Reached := True;
         end if;

      exception
         when others =>
               Report.Failed ("Unexpected Exception in Task_MD");
      end Task_MD; 

      --=====================


      -- This task has priority of Priority_HI, it calls the procedure;
      --
      task Task_HI_Proc is
         pragma Interrupt_Priority (Priority_HI);
      end Task_HI_Proc;
      -- 
      task body Task_HI_Proc is 
       
         -- This task calls a protected procedure whose declared ceiling is
         -- lower than the task's declared priority (no defaults). The
         -- ceiling check should raise Program_Error.
      begin
         Protected_Object.P_procedure;  -- not o.k.
         Expected_Exception_Not_Raised_in_Proc := True;
      exception
         when Program_Error =>
            null;  -- expected exception
         when others =>
            Unexpected_Exception_in_Proc := True;
      end Task_HI_Proc; 

      --=====================

      -- This task has priority of Priority_HI, it calls the function;
      --
      task Task_HI_Func is
         pragma Interrupt_Priority (Priority_HI);
      end Task_HI_Func;
      --
      task body Task_HI_Func is 

         -- This task calls a protected function whose declared ceiling is
         -- lower than the task's declared priority (no defaults). The
         -- ceiling check should raise Program_Error.
         Func_Return : natural;

      begin

         Func_Return := Protected_Object.P_function;  -- not o.k.
         Expected_Exception_Not_Raised_in_Func := True;

         -- This dummy routine with an external effect makes use
         -- of the result returned by the function.  This ensures that
         -- nothing will be optimized away.  The routine should not get
         -- executed 
         -- 
         if Func_Return = natural'Last then
            Unexpected_Code_Reached := True;
         end if;

      exception
         when Program_Error =>
            null;   -- expected exception
         when others =>
            Unexpected_Exception_in_Func := True;
      end Task_HI_Func; 

      --=====================

      -- This task has priority of Priority_HI it calls the entry;
      --
      task Task_HI_Ent is 
         pragma Interrupt_Priority (Priority_HI);
      end Task_HI_Ent; 
      -- 
      task body Task_HI_Ent is 
         -- This task calls a protected entry whose declared ceiling is
         -- lower than the task's declared priority (no defaults). The
         -- ceiling check should raise Program_Error.
      begin
         Protected_Object.P_entry;  -- not o.k.
         Expected_Exception_Not_Raised_in_Ent  := True;
      exception
         when Program_Error =>
            null;
         when others =>
            Unexpected_Exception_in_Ent  := True;
      end Task_HI_Ent; 

   begin
      null;
   end;  -- encapsulation

   if Expected_Exception_Not_Raised_in_Proc then
      Report.Failed ("Program_Error not raised in P_procedure");
   end if;

   if Unexpected_Exception_in_Proc  then
      Report.Failed ("Unexpected Exception in Task_HI_Proc");
   end if;

   if Expected_Exception_Not_Raised_in_Func then
      Report.Failed ("Program_Error not raised in P_function");
   end if;

   if Unexpected_Exception_in_Func  then
      Report.Failed ("Unexpected Exception in Task_HI_Func");
   end if;

   if Expected_Exception_Not_Raised_in_Ent  then
      Report.Failed ("Program_Error not raised in P_entry");
   end if;

   if Unexpected_Exception_in_Ent   then
      Report.Failed ("Unexpected Exception in Task_HI_Ent");
   end if;

   if Unexpected_Code_Reached then
      Report.Failed ("Unexpected code reached");
   end if;


   Report.Result;

end CXD3001;
