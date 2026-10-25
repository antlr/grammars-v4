-- CXDA002.A
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
--      Check that, in Ada.Synchronous_Task_Control, Suspend_Until_True does
--      suspend the task until the Suspension_Object is Set_True. Check that a
--      call on Suspend_Until_True will raise  Program_Error if another task
--      is waiting on the same  Suspension_Object. 
--
-- TEST DESCRIPTION:
--      One task calls Suspend_Until_True.  A second task sets the
--      suspension_object true and also a flag to say it has executed the code
--      to do so.  The first task checks the flag after the suspension.  If
--      the flag is not set then the second task did not execute showing that
--      the first task did not suspend properly.  Also in the second task call 
--      Suspend_Until_True specifying the same object and verify that
--      Program_Error  is raised
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Report;
with Ada.Synchronous_Task_Control;
with ImpDef;      -- Implementation Defined Constants

procedure CXDA002 is

   package ASTC renames Ada.Synchronous_Task_Control;

   Susp_Obj : ASTC.Suspension_Object;

begin

   Report.Test ("CXDA002", "Synchronous Task Control: Task suspension");

   declare -- encapsulate the test
      
      -- Protected object for communication between the two tasks
      -- 
      protected TC is
         function Enabling_Task_Has_Run return Boolean;
         procedure Set_Enabling_Task_Has_Run;
      private
         flag : Boolean := false;
      end TC;

      protected body TC is
         function Enabling_Task_Has_Run return Boolean is
         begin 
            return flag;
         end Enabling_Task_Has_Run;

         procedure Set_Enabling_Task_Has_Run is
         begin
            flag := true;
         end Set_Enabling_Task_Has_Run;
      end TC;

      task Suspending_Task;
      task Enabling_Task is
         entry Start;
      end Enabling_Task;


      -- This task suspends on the object
      task body Suspending_Task is
      begin 

         -- Start the second task which will, after a suitable delay,
         -- set the Suspension_Object true
         Enabling_Task.Start;

         ASTC.Suspend_Until_True (Susp_Obj);   -- task should suspend here

         if not TC.Enabling_Task_Has_Run then
            -- Enabling_Task_Has_Run will be set when the second task is 
            -- about to set the suspension_object true.  It is not set,
            -- so this indicates that the suspension did not take place or
            -- the object was set true prematurely.
            Report.Failed ("Suspending_Task was not suspended");
         end if;

      exception 
         when others =>
             Report.Failed ("Unexpected Exception in Suspending_Task");
      end Suspending_Task;
      

      -- This task will set the Suspension_Object True thus enabling the
      -- suspended task to continue
      task body Enabling_Task is
      begin 

         accept Start;

         -- Ensure that Suspending_Task does indeed suspend
         delay (Impdef.Minimum_Task_Switch); 
         
         declare -- exception scope
         -- Before releasing the first task we take advantage of the fact
         -- that this Suspension_Object is in use to make another check:
         -- Now verify that a call on Suspend_Until_True will raise 
         -- Program_Error as another task is waiting on the same 
         -- Suspension_Object
         begin
            ASTC.Suspend_Until_True (Susp_Obj);
            Report.Failed ("Program Error not raised");
            -- Another possible failure could occur here:  the task actually
            -- does suspend even though the object is in use.  In which case
            -- the test will hang as no Set_True will be issued.
         exception
            when Program_Error => null;
            when others => Report.Failed ("Wrong Exception Raised");
         end;  -- exception scope

         -- Delay long enough to allow the first task to (erroneously)
         -- continue after the call to Suspend_Until_True;
         --
         delay (Impdef.Minimum_Task_Switch * 3); 
         -- 
         TC.Set_Enabling_Task_Has_Run;  -- This is checked by the first task

         ASTC.Set_True (Susp_Obj);

      exception 
         when others =>
             Report.Failed ("Unexpected Exception in task Enabling_Task");
      end Enabling_Task;

   begin  -- declare

      null;

   end;   -- declare 

   Report.Result;

end CXDA002;
