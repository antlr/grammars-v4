-- CXD1004.A
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
--      Check that during activation, a task being activated inherits the
--      active priority of its activator (in this case the activator's
--      base priority).  Check that, if this priority is higher than the 
--      base priority of the activated task, this base priority remains
--      unchanged.
--      
-- TEST DESCRIPTION:   
--      Three separate instances of a task type T1 are generated, one by the
--      Main with the Default Base priority, one by a task (Task_3Q) with  a
--      base priority high in the normal priority range and one by a task
--      who's base priority is in the Interrupt range.  T1 is designed to
--      trap its activation priority and store the same in the
--      Resulting_Priorities array when the body of the task is executed. 
--      The instances of the task are distinguished by discriminants.
--      
--      Two levels of activation priority can be verified - Interrupt
--      priority and less-than interrupt priority.  These are called
--      Hi_Low_Priority High/Low.  We can determine which of these applies
--      by calling a Protected Object who's Ceiling priority has been set
--      to System.Priority'Last.  If the current activation priority is not
--      greater than the ceiling (Low) the call will be honored.  If it is
--      greater than the ceiling (High) this is a violation of the Ceiling
--      Priority of the Protected Object and Program_Error will be raised.
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
--      02 Nov 95   SAIC    Fixed priority values for ACVC 2.0.1
--
--!

-----------------------  Configuration Pragmas --------------------

pragma Locking_Policy (Ceiling_Locking);
   
-------------------  End of Configuration Pragmas --------------------


with System;
with Report;
with Ada.Dynamic_Priorities;

procedure CXD1004 is
 
   package ADP renames Ada.Dynamic_Priorities;
   
   Priority_1Q : constant System.Priority :=
                            (System.Priority'First + 5);
   Priority_2Q : constant System.Priority :=
                            (System.Priority'First + System.Priority'Last)/2;
   Priority_3Q : constant System.Priority :=
                            (Priority_2Q + 5);
   


   type Task_Number is range 1..3;  -- limit this test to three tasks
   type Hi_Lo_Priority is (Low, High);

   Get_High_Lo_Unexpected_Exception : Boolean := False;
   Interrupt_Task_Unexpected_Exception : Boolean := False;

   -- Store the resulting activation priorities of the tasks 
   Resulting_Priorities : array (Task_Number) of Hi_Lo_Priority;

   -- This protected object has a ceiling of Priority'Last.  Calls to 
   -- Check will be accepted if the caller has a priority in the regular 
   -- priority range, they will be rejected if the caller has a higher
   -- priority (Program_Error will be raised)
   --
   protected For_Ceiling is 
      Pragma priority (System.Priority'Last);
      procedure Check;
      function Number_of_Calls return natural;
   private
      Count : natural := 0;
   end For_Ceiling;
   protected body For_Ceiling is
      procedure Check is
      begin
         -- We must ensure that the calls to this Protected_Object
         -- are not optimized away, otherwise the test would be
         -- invalid.  Count is also used by the function which is 
         -- checked at the end of the test.  
         Count := Count +1;

      end Check;

      function Number_of_Calls return natural is
      begin 
         return Count;
      end Number_of_Calls;

   end For_Ceiling;


begin -- CXD1004

   Report.Test ("CXD1004", "During Activation, a task inherits " &
                                         "the active priority of its parent");

   declare     -- encapsulate the test
      
      -- Determine the active priority of the task executing this
      -- function
      --
      function Get_Hi_Lo_Priority return Hi_Lo_Priority is
      begin
         -- check the current Active priority by calling the P.O
         For_Ceiling.Check;
         -- the call did not raise exception - we are below the ceiling
         return Low;
      exception
         when Program_Error =>
            -- The call has violated the ceiling of the Protected Object
            -- so the current active priority is High
            return High;
         when others =>
            Get_High_Lo_Unexpected_Exception := True;
            raise;  
      end Get_Hi_Lo_Priority; 


      -- The activation priority of tasks of this type is checked in 
      -- various circumstances
      task type T1 (Numb : Task_Number) is
         -- All these tasks will have a base priority of Priority_1Q.
         Pragma Priority ( Priority_1Q );
      end T1;
      
      task body T1 is
         Thus_Task_Number : Task_Number := Numb;
         -- During activation capture the active priority of the task
         Activation_Priority : Hi_Lo_Priority := Get_Hi_Lo_Priority;
      begin 
         
         -- Plug the Activation Priority into the appropriate slot
         -- for this task in the Result array
         Resulting_Priorities (Thus_Task_Number) := Activation_Priority;

         -- Now check the current base priority of the task.  We are 
         -- executing the body and the task has a Pragma Priority which
         -- overrides the base priority of the creating task.  So, whatever
         -- the activation priority might have been it should not affect the
         -- base priority
         if ( ADP.Get_Priority /= Priority_1Q ) then 
            Report.Failed ("Base Priority of T1 is incorrect");
         end if;

      exception
         when others =>
            Report.Failed ("Unexpected Exception in T1");
      end T1;

      --=================================================

      task Task_3Q is
         pragma priority ( Priority_3Q );
      end Task_3Q;

      task body Task_3Q is
         
         -- Now create a task.  We are currently in a task who's priority 
         -- has been set by pragma to  Priority_3Q so this should, in turn,
         -- be the activation priority of TT2 
         --
         TT2 : T1 (2);    -- Task_Number passed in as a discriminant

      begin
         null;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Task_3Q");
      end Task_3Q;

      --=======================

      task Interrupt is
         pragma Interrupt_Priority (System.Interrupt_Priority'First);
      end Interrupt;

      task body Interrupt is
         -- Now create a task.  We are currently in an Interrupt priority
         -- task so this should, in turn, be the activation priority 
         -- of TT3 
         --
         TT3 : T1 (3);    -- Task_Number passed in as a discriminant

      begin
         null;
      exception
         when others =>
            Interrupt_Task_Unexpected_Exception  := True;
      end Interrupt;

      --=======================

      -- Now create a task.  We are in the activation phase of the Main
      -- procedure which does not have a Pragma Priority so the base
      -- priority will be the default ( Priority_2Q ).  Thus the activation
      -- priority of TT1 should be Priority_2Q.
      --
      TT1 : T1 (1);    -- Task_Number passed in as a discriminant

      --=======================

   begin 
      null;
   end;     -- encapsulation
   
   -- All tasks have terminated.  Check the activation priority results
   --
   -- Task TT1 should have had a Low priority ( Priority_2Q  - the default )
   if Resulting_Priorities (1) = High then
      Report.Failed ("Task number 1 had incorrect priority");
   end if;

   -- Task TT2 should have had a Low priority ( Priority_1Q )
   if Resulting_Priorities (2) = High then
      Report.Failed ("Task number 2 had incorrect priority");
   end if;

   -- Task TT3 should have had a High priority ( Interrupt )
   if Resulting_Priorities (3) = Low then
      Report.Failed ("Task number 3 had incorrect priority");
   end if;

   if For_Ceiling.Number_of_Calls /= 2 then 
      -- Two Low priority calls should have been accepted, the High
      -- priority call should have been rejected.  If this number is correct
      -- we know the calls have not been optimized away
      --
      Report.Failed ("Incorrect number of calls accepted in Check");
   end if;


   -- report any errors that may have occurred while we were
   -- running at an interrupt priority

   if Get_High_Lo_Unexpected_Exception  then
      Report.Failed ("Unexpected Exception in Get_Hi_Lo_Priority");
   end if;

   if Interrupt_Task_Unexpected_Exception  then
      Report.Failed ("Unexpected Exception in task Interrupt");
   end if;

   Report.Result;


end CXD1004;
