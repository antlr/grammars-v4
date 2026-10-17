-- B952002.A
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
--      Check that the body of a protected entry must have an entry barrier.
--
--      Check that if an entry identifier appears at the end of an entry body
--      it  repeats the defining identifier of the entry or the entry family
--
-- TEST DESCRIPTION:
--      Define two protected objects.  In the first, omit the barrier
--      condition in the body.  In the second specify two entries: one a
--      single entry and  the other a family.  Use an undefined identifier in
--      the "end" of one of them (typical typo.); mismatch the "end"
--      identifier in the other.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


procedure B952002 is

   type Item_Type is (Single_Occupant, Multi_Occupant);
   
   --=========================

   protected Ramp is
      entry Wait_at_Meter;
   end Ramp;

   protected body Ramp is
      entry Wait_at_Meter is                                          -- ERROR:
      begin                                          -- Must have entry barrier
         null;
      end Wait_at_Meter;
   end Ramp;

   --=========================

   protected Ramp_01 is 
      -- Single
      entry Wait_at_Meter;
      -- Family
      entry Hold_By_Priority (Item_Type);
   end Ramp_01;

   protected body Ramp_01 is

      entry Wait_at_Meter when true is 
      begin
         null;
      end Wait_on_Meter;                                             -- ERROR: 
                                                       -- Undefined Identifier
      entry Hold_by_Priority 
        (for AP in Item_Type) when true is 
      begin
         null;
      end Wait_at_Meter;                                              -- ERROR:
                                                       -- Mismatched Identifier
   end Ramp_01;

   --=========================

begin
   null;
end B952002;
