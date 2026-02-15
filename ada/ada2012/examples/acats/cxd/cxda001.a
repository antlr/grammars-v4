-- CXDA001.A
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
--      Check that, in Ada.Synchronous_Task_Control, Set_True and Set_False 
--      alter the state of a Suspension_Object appropriately.  Check that 
--      Current_State returns the expected state.  Check that the initial 
--      value of a Suspension_Object is set to false.
--
-- TEST DESCRIPTION: 
--      In the Main subprogram use Current_State to verify the state of a
--      Suspension_Object initially and after the use of Set_True and 
--      Set_False.   
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


with Report;
with Ada.Synchronous_Task_Control;

procedure CXDA001 is

   package ASTC renames Ada.Synchronous_Task_Control;
   Susp_Obj : ASTC.Suspension_Object;

begin

   Report.Test ("CXDA001", "Synchronous Task Control: basic functionality");

   -- Initial state must be false
   if ASTC.Current_State(Susp_Obj) then
      Report.Failed ("Suspension_Object not initialized to False");
   end if;

   ASTC.Set_True( Susp_Obj );

   if not ASTC.Current_State(Susp_Obj) then
      Report.Failed ("Set_True failed");
   end if;

   ASTC.Set_False( Susp_Obj );

   if ASTC.Current_State(Susp_Obj) then
      Report.Failed ("Set_False failed");
   end if;

   Report.Result;

end CXDA001;
