-- B940006.A
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
--      Check that component declarations are only allowed in the private
--      part of protected objects
--
-- TEST DESCRIPTION: 
--      Declare a variable in the main part of the specification (as one
--      can in a package).  
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      10 Nov 95   SAIC    ACVC 2.0.1
--                          Fixed ARM references.
--                          Removed assignment to PO component from within
--                            protected function.
--
--!


procedure B940006 is

   -- Variable declared outside the private part
   --
   protected Object is
      -- declare a variable as we would in a package
      Variable_1 : integer;                                          -- ERROR:
                                                        -- not in private part
      function Good_Function return Boolean;

   private
      Variable_2 : integer; -- O.K.
   end Object;

   -----------

   protected body Object is
      function Good_Function return Boolean is
      begin
         return true;
      end Good_Function;
   end Object;

begin
  null;

end B940006;
