-- B940002.A 
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
--      Check that a protected_element_declaration within the private part
--      of a protected type must be a component_declaration (if it is not
--      a protected_operation_declaration).
--      Specifically: a constant component is not allowed
--                    a type declaration is not allowed 
-- 
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


procedure B940002 is
   
   
   protected Hold_1 is
      procedure Set_Current_Priority;
   private
      Maximum_Queue_Length : constant integer := 25;                 -- ERROR: 
                                            -- constant components not allowed 
                                            -- in protected types
   end Hold_1;

   protected body Hold_1 is
      procedure Set_Current_Priority is
      begin
         null;
      end Set_Current_Priority;
   end Hold_1;

   --==================================
   
   protected Hold_2 is
      procedure Set_Current_Priority;
   private
      type Application_Priority is (Low, Medium, High);              -- ERROR: 
                                            -- type declaration is not allowed 
                                            -- in protected types
   end Hold_2;

   protected body Hold_2 is
      procedure Set_Current_Priority is 
      begin
         null;
      end Set_Current_Priority;
   end Hold_2;

begin

   null;

end B940002;
