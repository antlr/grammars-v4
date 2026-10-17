-- CXD1006.A
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
--      Check that if there is no expression in an Interrupt_Priority
--      pragma that the priority value is Interrupt_Priority'Last.
--
-- TEST DESCRIPTION:
--      This test declares a task that contains a pragma 
--      Interrupt_Priority.  The package Ada.Dynamic_Priorities is
--      used to query the priority of the task.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Real-Time Systems Annex, and which allow tasks to have
--      priorities in the interrupt priority range (D.2.1(10)).
--
--
-- CHANGE HISTORY:
--      04 AUG 95   SAIC    Initial version
--      25 NOV 98   RLB     Expanded applicability criteria.
--
--!


with System;
with Report;
with Ada.Dynamic_Priorities;
procedure CXD1006 is
  Verbose : constant Boolean := False;
begin
  Report.Test ("CXD1006",
               "Check the priority of a task containing" &
               " an Interrupt_Priority pragma without the" &
               " optional expression");
  declare
    task Check_It is
        pragma Interrupt_Priority; -- N/A => Error.
    end Check_It;

    task body Check_It is
      P : System.Any_Priority;
    begin
      P := Ada.Dynamic_Priorities.Get_Priority;
      if P /= System.Interrupt_Priority'Last then
        Report.Failed ("expected task priority to be" &
               System.Any_Priority'Image (System.Interrupt_Priority'Last) &
               " but the priority was" &
               System.Any_Priority'Image (P) );
      else
        if Verbose then
          Report.Comment ("priority is" &
                          System.Any_Priority'Image (P) &
                          " which is System.Interrupt_Priority'Last" );
        end if;
      end if;
    end Check_It;
  begin
    null;
  end;

  Report.Result;
end CXD1006;
