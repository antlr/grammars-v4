-- B952003.A
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
--      Check that, in the body of a protected entry, the 
--      entry_index_specification must be enclosed in parentheses.
--
-- TEST DESCRIPTION:
--      This is a syntax test. The example definition is taken from C954023
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


procedure B952003 is

   
   type App_Priority     is (Low, Medium, High);
   type Priority_Block   is array (App_Priority) of Boolean;

   type Transaction_Record;
   type acc_Transaction_Record is access Transaction_Record;
   type Transaction_Record is 
      record
         ID               : integer := 0;
         Priority         : App_Priority := High;
         Account_Number   : integer := 0;
         Stock_Number     : integer := 0;
         Quantity         : integer := 0;
         Return_Value     : integer := 0;
      end record;


   protected Hold is
      -- Family of entry queues indexed by App_Priority
      entry Wait_for_Underload (App_Priority)
                                  (Transaction : acc_Transaction_Record);
   private
      Release : Priority_Block := (others => false);
   end Hold;
   --
   --
   protected body Hold is
         
      -- This is a family of entry queues indexed by App_Priority
      entry Wait_for_Underload  
               for AP in App_Priority                                 -- ERROR:
                                                         -- Missing Parentheses
               (Transaction : acc_Transaction_Record)
               when Release(AP) is
      begin
         null;
      end Wait_for_Underload;
   
   end Hold;

begin
   null;
end B952003;
