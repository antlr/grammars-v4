-- BB10001.A
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
--      Check that separate exception handlers for Constraint_Error and 
--      Numeric_Error are not allowed within a handled sequence of 
--      statements.
--
-- TEST DESCRIPTION:
--      This test is designed to verify that Numeric_Error is now defined
--      as a rename of Constraint_Error, and that it will not be legal to 
--      have separate handlers for Constraint_Error and Numeric_Error within
--      the same handled sequence of statements.
--      Note also that the rules for uniqueness in exception handlers have 
--      been changed to allow the same exception to be named more than once
--      by a given exception handler.
--      Note: This test is designed to ensure correct Numeric_Error processing
--            for the transition to Ada 9X.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

procedure BB10001 is

   I : Integer;

   procedure BB10001_0 is
      I : Integer;
   begin
      I := 1;
   exception
      when Numeric_Error | Constraint_Error =>                          -- OK. 
         null;                                             -- All on one line.
   end BB10001_0;


begin  -- BB10001.


   declare

      function BB10001_1 return Integer is
         I : Integer := 1;
      begin
         return (I*I);
      exception                                   
         when Constraint_Error =>                                       -- OK.
            null;                                    -- Reverse CE , NE order.
         when Numeric_Error    =>                                    -- ERROR:
            null;                              -- Separate handler for rename.
      end BB10001_1;

   begin
      raise Constraint_Error;
   exception
      when others =>

         begin
            raise Numeric_Error;
         exception
            when Constraint_Error | Numeric_Error =>                    -- OK.
               null;                    -- Reverse CE | NE order, on one line.
         end;                                              
   end;


   declare

      Renamed_Exception : exception renames Constraint_Error;

   begin

      declare
         Renamed_Again     : exception renames Renamed_Exception;
      begin
         I := I + 1;
      exception
         when Renamed_Exception |
              Numeric_Error =>                                          -- OK.
            null;
         when Renamed_Again | Constraint_Error =>                    -- ERROR:
            null;                              -- Separate handler for rename.
      end;

   end;


end BB10001;
