-- B951001.A
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
--      Check that the body of a protected function cannot have an internal
--      call to a protected procedure.
--
-- TEST DESCRIPTION:
--      Internal calls from functions to procedures within a protected object 
--      declared as an anonymous type and within one declared as a type are 
--      checked.  Each one of the calls should be flagged as an error.
--
--
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!


procedure B951001 is

      protected Anonymous_Object is
         function Func_in_Anon return integer;
         procedure Proc_in_Anon;
      end Anonymous_Object;

      protected type Object_of_Type is
         function Func_in_OT return integer;
         procedure Proc_in_OT; 
      end Object_of_Type;

      --==============================================================

      -- Check a call from within a protected object declared as 
      -- an anonymous type

      protected body Anonymous_Object is

         function Func_in_Anon return integer is
            int : integer := 1;
         begin 
            Proc_in_Anon;                                            -- ERROR:
                                                        -- Illegal call 
                                                        -- Internal, Anonymous
            return int;
         end Func_in_Anon;

         procedure Proc_in_Anon is
         begin
            null; 
         end Proc_in_Anon;

      end Anonymous_Object;

      --==============================================================

      -- Check a call from within a protected object declared as 
      -- a type

      protected body Object_of_Type is

         function Func_in_OT return integer is
            int : integer := 1;
         begin 
            Proc_in_OT;                                              -- ERROR:
                                                          -- Illegal call, 
                                                          -- Internal, of type
            return int;
         end Func_in_OT;

         procedure Proc_in_OT is
         begin
            null; 
         end Proc_in_OT;

      end Object_of_Type;

      --==============================================================

begin

   null;

end B951001;
