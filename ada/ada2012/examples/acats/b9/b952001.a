-- B952001.A
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
--      Check that the name that denotes the formal parameter of an 
--      entry body is not allowed within the entry barrier
--
-- TEST DESCRIPTION:
--      Three protected bodies are defined.  One with a single entry, one with
--      a family of entries and a third with a function call in the  barrier
--      of a single entry. Separate  protected objects are used (rather than
--      one having all entries) for ease of splitting if  required
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


procedure B952001 is

   subtype Priority_Level is integer range 1..5;
   type    Item_Type      is (Credit, Debit);
   Current_Priority_Level : Priority_Level;  

   function Priority_OK (Item_Priority : Priority_Level) return Boolean is 
   begin
      return  (Item_Priority > Current_Priority_Level); 
   end Priority_OK;

   --===================
   
   -- First, define the Protected object with a single entry
   protected Hold is
      entry Queue (Item_Priority : Priority_Level);
   end Hold;
   
   -- Now define the Protected object with an entry family
   protected Hold_Family is
      entry Queue (Item_Type) (Item_Priority : Priority_Level);
   end Hold_Family;

   -- Now define the Protected object with a single entry which uses a
   -- function call in the barrier
   protected Hold_With_Func is
      entry Queue (Item_Priority : Priority_Level);
   end Hold_With_Func;
   
   --===================
   
   -- Single entry
   protected body Hold is
      entry Queue (Item_Priority : Priority_Level) 
        when  Item_Priority > Current_Priority_Level  is             -- ERROR:
                                                           -- Formal parameter 
                                                           -- in entry barrier
      begin
         null;
      end Queue;
   end Hold;

   --===================
   
   -- Family of entries
   protected body Hold_Family is
      entry Queue (for I in Item_type) (Item_Priority : Priority_Level) 
        when (Item_Priority > Current_Priority_Level) is             -- ERROR:
                                                           -- Formal parameter 
                                                           -- in entry barrier
      begin
         null;
      end Queue;
   end Hold_Family;

   --===================
   
   -- Entry with function in barrier condition
   protected body Hold_With_Func is
      entry Queue (Item_Priority : Priority_Level) 
        when Priority_OK (Item_Priority)is                           -- ERROR:
                                                           -- Formal parameter 
                                                           -- in entry barrier
      begin
         null;
      end Queue;
   end Hold_With_Func;
   
   --===================

begin
   null; 
end B952001;
