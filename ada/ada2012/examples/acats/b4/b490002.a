-- B490002.A
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
--      Check that a static string expression that is the result of a 
--      catenation is illegal if it has length greater than that permitted by 
--      the expected type.  Check that it is illegal to assign a null string 
--      literal to an object whose lower bound is equal to the lower bound of 
--      the base range of the index type.
--
-- TEST DESCRIPTION:
--      This transition test creates examples where catenations of types 
--      Character and Wide_Character are used.  This test also creates 
--      examples where a null string literal is assigned as an actual parameter
--      to a procedure call, objects initializations, and allocator.
--
--      Inspired by C45345A.ADA and C42011A.ADA.
--
--
-- CHANGE HISTORY:
--      01 Feb 96   SAIC    Initial version for ACVC 2.1.
--      19 Oct 96   SAIC    ACVC 2.1: Modified Proc1 case.
--
--!

procedure B490002 is

   subtype Small_Int is Integer range 1 .. 5;
   type    Array_String is array (Small_Int range <>) of Character;
   A1 : Array_String (1 .. 5);
   A2 : Array_String (1 .. 4);

   procedure Proc1 (P : Array_String) is
      begin
         A2 := P & "F";                                               
      end Proc1;

   ----------------------------------------------------------------------
   type Wide_String is array (Small_Int range 1 .. 4) of Wide_Character;
   S1 : constant Wide_String := "FFFE"; 
   S2 : constant Wide_String := "FFFF";
   S3 : Wide_String renames S1;
   S4 : Wide_String;

   ----------------------------------------------------------------------
   subtype Int is Integer range Integer'First .. Integer'First + 3;
   type    Array_Str is array (Int range <>) of Character;
   type    Acc_Array_Str is access Array_Str;

   C1 : constant Array_Str   := "1234";
   C2 : constant Array_Str   := "5678";

   I1 : Acc_Array_Str;

   I2 : Array_Str            := "ABCD";

   procedure Proc2 (P : Array_Str) is
      begin
         I2 := P & "N";                                               
      end Proc2;

   ----------------------------------------------------------------------

   I3 : Array_Str            := "";                                  -- ERROR:
                                                        -- Value not in range.

   I4 : constant Array_Str   := "";                                  -- ERROR:
                                                        -- Value not in range.

   I5 : Array_Str            := "ABCD" & "XYZ";                      -- ERROR:
                                                        -- Value not in range.

   I6 : Array_Str            := C1 & C2;                             -- ERROR:
                                                        -- Value not in range.

begin

   A1 := "ABCD" & "EF";                                              -- ERROR:
                                                     -- String value too long.

   A1 := "ABCDE" & "F";                                              -- ERROR:
                                                     -- String value too long.

   S4 := S1 & S2;                                                    -- ERROR:
                                                     -- String value too long.

   S4 := S1 & "F";                                                   -- ERROR:
                                                     -- String value too long.

   S4 := S1 & S3;                                                    -- ERROR:
                                                     -- String value too long.

   Proc1 ("");                                                       -- OK.

   Proc1 ("ABC" & "DEFG");                                           -- ERROR:
                                                        -- Value not in range.

   Proc2 ("");                                                       -- ERROR:
                                                        -- Value not in range.

   Proc2 (C1 & C2);                                                  -- ERROR:
                                                        -- Value not in range.

   I1 := new Array_Str'("");                                         -- ERROR:
                                                        -- Value not in range.

   if Array_Str'("ABCD" & "DEFG") = "ABCD" & "DEFG" then             -- ERROR:
                                                        -- Value not in range.
      null;
   end if;

end B490002;
