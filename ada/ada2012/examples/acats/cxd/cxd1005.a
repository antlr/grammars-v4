-- CXD1005.A
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
--      Check that, during activation, a task being activated inherits
--      the active priority of its activator.  Specifically, check when the 
--      active priority of the activator is higher than the activator's
--      Base Priority.  Check that if the priority of the activated task
--      is higher than its base priority, the base priority remains unchanged.
--
-- TEST DESCRIPTION:   
--      A task with Base Priority in the interrupt range enters a rendezvous
--      with one whose base is in the normal priority range.  During the
--      rendezvous, which will be performed at the interrupt priority, a
--      task is created (Activating_Task). This task should then be
--      activated at interrupt priority.  During activation the task notes
--      its current active priority (High or Low - see below) and  preserves
--      this for later check in the body.  Also the body of the task 
--      verifies that it is running at its own base priority rather than the 
--      priority of its activation.
--      
--      Two levels of activation priority can be verified - Interrupt
--      priority and  less than interrupt priority.  These are
--      Hi_Low_Priority High/Low.  We can determine which of these applies
--      by calling a Protected Object who's  Ceiling priority has been set
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
--      02 Nov 95   SAIC    Fixed priority problems for ACVC 2.0.1
--
--!


-----------------------  Configuration Pragmas --------------------

Pragma Locking_Policy (Ceiling_Locking);

-------------------  End of Configuration Pragmas --------------------

   
with System;
with Report;
with Ada.Dynamic_Priorities;

procedure CXD1005 is
 
   package ADP renames Ada.Dynamic_Priorities;
   
   Priority_1Q : constant System.Priority :=
                            (System.Priority'First + 5);
   Priority_3Q : constant System.Priority :=
                            (System.Default_Priority + 5);

   type Hi_Lo_Priority is (Low, High);

   Unexpected_Exception_In_Interrupt_Task : Boolean := False;
   Unexpected_Exception_in_Get_Hi_Lo : Boolean := False;

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


begin -- CXD1005

   Report.Test ("CXD1005", "Check that during Activation a task inherits " &
                                         "the active priority of its parent");

   declare     -- encapsulate the test
      

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
            Unexpected_Exception_in_Get_Hi_Lo := True;
            raise;  
      end Get_Hi_Lo_Priority; 



      
      task Task_3Q is
         pragma priority ( Priority_3Q );
         entry E1;
      end Task_3Q;

      task body Task_3Q is
      begin
         accept E1 do
            declare
         
               -- Now create a task.  The active priority at this point is
               -- that of the rendezvous 
               task Activating_Task is 
                  Pragma Priority ( Priority_1Q );
               end Activating_Task;
      
               task body Activating_Task is     

                  -- During activation capture the active priority of the task
                  Activation_Priority : Hi_Lo_Priority := Get_Hi_Lo_Priority;

               begin 
                  -- The activation priority should be that of the creating 
                  -- task at the time of creation.  We were in the 
                  -- rendezvous with the Interrupt task at the time.
                  --
                  if Activation_Priority /= High then
                     Report.Failed
                         ("Activation Priority of Activating_Task incorrect");
                  end if;
                  -- Now check the current base priority of the task.  
                  -- We are executing the body and the task has a 
                  -- Pragma Priority which overrides the base priority of 
                  -- the creating task. So, whatever the activation priority
                  -- might have been it should not affect the base priority
                  --
                  if ( ADP.Get_Priority /= Priority_1Q ) then 
                     Report.Failed 
                            ("Base Priority of Activating_Task is incorrect");
                  end if;
         
               exception
                  when others =>
                     Report.Failed 
                            ("Unexpected Exception in Activating_Task");
               end Activating_Task;
      
            begin
               null;
            end;   -- declare block
         end E1;
      exception
         when others =>
            Report.Failed ("Unexpected Exception in Task_3Q");
      end Task_3Q;

      --=========================

      task Interrupt is
         pragma Interrupt_Priority (System.Interrupt_Priority'First);
      end Interrupt;

      task body Interrupt is
      begin

         -- Rendezvous with Task_3Q.  Currently we are at Interrupt 
         -- priority and the rendezvous will be executed at the same
         --
         Task_3Q.E1;

      exception
         when others =>
            Unexpected_Exception_In_Interrupt_Task := True;
      end Interrupt;


   begin    -- encapsulation
      null;
   end;
   
   -- In this test the fact that High is returned and verified in 
   -- the body of Activating_Task shows that the call to Check was
   -- not optimized away, however we make this call to Number_of_Calls
   -- to aid in an optimizer foil.  If the optimizer were to remove
   -- the call we would get a false fail
   --
   if For_Ceiling.Number_of_Calls /= 0 then 
      -- The high priority call should have been rejected. 
      --
      Report.Failed ("Incorrect number of calls accepted in Check");
   end if;

   if Unexpected_Exception_In_Interrupt_Task then
      Report.Failed ("Unexpected Exception in task Interrupt");
   end if;

   if Unexpected_Exception_in_Get_Hi_Lo  then
      Report.Failed ("Unexpected Exception in Get_Hi_Lo_Priority");
   end if;

   Report.Result;


end CXD1005;
