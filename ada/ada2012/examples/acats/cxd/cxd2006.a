-- CXD2006.A
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
--      Check that priority ceases to be inherited as soon as the 
--      condition calling for the inheritance no longer exists.
--
-- TEST DESCRIPTION:
--      This test has one task that, by default, will run at the
--      default priority.  A second task that runs at an interrupt
--      priority is used to cause the first task to inherit a 
--      higher priority.  The active priority of the first task
--      is checked around events that change the priority inheritance.
--
-- SPECIAL REQUIREMENTS
--      The implementation must process configuration pragmas which 
--      are not part of any Compilation Unit;  the method employed
--      is implementation defined.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Real-Time Systems Annex.
--
--
-- CHANGE HISTORY:
--      19 SEP 95   SAIC    Initial version
--      26 OCT 95   SAIC    Incorporated reviewer comments.
--      28 DEC 95   SAIC    Fixed bug reported by reviewer.
--      11 OCT 96   SAIC    Fixed multi-processor race condition
--
--!

--------------------------- Configuration Pragmas ---------------------

pragma Locking_Policy (Ceiling_Locking);
pragma Task_Dispatching_Policy (FIFO_Within_Priorities);

---------------------- End of Configuration Pragmas -------------------


with System;
with Report;
with ImpDef;
with Ada.Task_Identification;
procedure CXD2006 is
  Verbose : constant Boolean := False;

  -- the following flag is used to relay information
  -- about the outcome of the priority check back to
  -- the main task.  This is done so we won't be
  -- trying to call Report.Failed while running at
  -- an interrupt priority.
  Priority_Check_Ok : Boolean := False;
  pragma Volatile (Priority_Check_Ok);

  package TID renames Ada.Task_Identification;

  -- the following protected object is used to detect 
  -- an active priority in the interrupt_priority range.
  protected Priority_Normal is
    -- priority ceiling is below interrupt_priority range.
    -- Therefore, any attempt to call Check while running 
    -- with a priority in the interrupt_priority range
    -- will result in a program_error.
    procedure Check;
    -- Get_Count is to thwart optimizing the PO away
    function Get_Count return Integer;
  private
    Count : Integer := 0;
  end Priority_Normal;

  protected body Priority_Normal is
    procedure Check is
    begin
      Count := Count + 1;
    end Check;
   
    function Get_Count return Integer is
    begin
      return Count;
    end Get_Count;
  end Priority_Normal;
    
  -- if the current active priority is in the interrupt range
  -- then the check passes.
  -- Otherwise, the check fails.
  procedure Assert_High_Priority is
  begin
    Priority_Normal.Check;  -- expect exception here
    Priority_Check_Ok := False;
  exception
    when Program_Error =>
      Priority_Check_Ok := True;
  end Assert_High_Priority;

    
  -- if the current active priority is in the normal range
  -- then the check passes.
  -- Otherwise, the check fails and Msg is to be displayed as the
  -- failure message.
  procedure Assert_Normal_Priority is
  begin
    Priority_Normal.Check;  
    Priority_Check_Ok := True;
  exception
    when Program_Error =>  -- priority above ceiling
        Priority_Check_Ok := False;
  end Assert_Normal_Priority;


  procedure Check_It (Msg : String) is
  begin
    if Priority_Check_Ok then
      if Verbose then
        Report.Comment ("passed " & Msg);
      end if;
    else
      Report.Failed (Msg);
    end if;
    -- fail safe for the next check
    Priority_Check_Ok := False;
  end Check_It;


  task Inherits is
    -- this task gets all its priority from inheritance
    pragma Priority (System.Priority'First);

    -- entry used to cause inheritance
    entry Inherit_Priority;
    entry Complete_Rendezvous;

    -- verify that active priority is normal
    entry Normal;

    -- verify that active priority is interrupt (high)
    entry High;
  end Inherits;


  task body Inherits is
  begin
    loop
      select
        accept Inherit_Priority do
          -- stay in this rendezvous until we are told to
          -- complete the rendezvous
          IP_Loop: loop
            select
              accept Normal do
                Assert_Normal_Priority;
              end Normal;
            or
              accept High do
                Assert_High_Priority;
              end High;
            or
              accept Complete_Rendezvous;
              exit IP_Loop;
            end select;
            
          end loop IP_Loop;
        end Inherit_Priority;
      or
        accept Normal do
          Assert_Normal_Priority; 
        end Normal;
      or
        accept High do
          Assert_High_Priority;
        end High;
      or
        terminate;
      end select;
    end loop;
  end Inherits;

----------------------------------------------------------

  task type Hi_Priority_Task is
    pragma Interrupt_Priority (System.Interrupt_Priority'First);
    entry Cause_Inheritance (Me : out TID.Task_ID);
  end Hi_Priority_Task;

  task body Hi_Priority_Task is
    Myself : constant TID.Task_ID := TID.Current_Task;
  begin
    accept Cause_Inheritance (Me : out TID.Task_ID) do
      Me := Myself;
    end Cause_Inheritance;

    -- This task will rendezvous with Inherits thus raising the
    -- priority of that task.
    Inherits.Inherit_Priority;
    if Verbose then
      Report.Comment ("Hi_Priority_Task is done");
    end if;
  end Hi_Priority_Task;

begin
  Report.Test ("CXD2006",
               "Check that priority ceases to be inherited" &
               " as soon as the conditions for inheritance" &
               " no longer exist");

  declare  -- encapsulation
    Hi : Hi_Priority_Task;
    ID_Hi : TID.Task_ID;
  begin    -- encapsulation
    if Verbose then
      Report.Comment ("deinheritance after leaving entry queue");
    end if;
    
    -- just check that the priority is as expected at the start
    Inherits.Normal;
    Check_It ("initial priority");

    Hi.Cause_Inheritance (ID_Hi);
    delay ImpDef.Clear_Ready_Queue;
    -- at this point Hi should have caused Inherits to inherit
    -- a high priority 
    Inherits.High;
    Check_It ("high priority task waiting");

    Inherits.Complete_Rendezvous;
    delay ImpDef.Clear_Ready_Queue;

    -- Hi is no longer contributing to Inherits 
    -- priority so Inherits should be back to normal.
    Inherits.Normal;
    Check_It ("revert back to normal priority");
  end;     -- encapsulation


  -- don't let the compiler think we don't care about the value
  -- in the protected object
  if Priority_Normal.Get_Count /= 2 then
    Report.Failed ("protected object count is messed up");
  end if;

  Report.Result;
end CXD2006;
