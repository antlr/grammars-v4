-- BXA8001.A
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
--      Check that Append_File mode has not been added to package Direct_IO.
--
-- APPLICABILITY CRITERIA: 
--      Applicable to all implementations that support Direct_IO operations.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Ada.Direct_IO;

procedure BXA8001 is

   subtype String_Data_Type    is String (1 .. 80);
   type    Numeric_Data_Type   is range 1 .. 1024;
   type    Composite_Data_Type is array (1 .. 2) of String_Data_Type;

   type File_Data_Type is record
      Data_Field_1 : String_Data_Type;
      Data_Field_2 : Numeric_Data_Type;
      Data_Field_3 : Composite_Data_Type;
   end record;

   package Dir_IO is new Ada.Direct_IO (File_Data_Type);

   Data_File        : Dir_IO.File_Type;
   The_File_Mode    : Dir_IO.File_Mode;
   Illegal_Variable : Dir_IO.File_Mode := Dir_IO.Append_File;       -- ERROR:
   Filename         : constant String  := "TESTFILE.";

begin

   Dir_IO.Create (Data_File, Dir_IO.Append_File, Filename);         -- ERROR:
   Dir_IO.Open   (Data_File, Dir_IO.Append_File, Filename);         -- ERROR:
   Dir_IO.Reset  (Data_File, Dir_IO.Append_File);                   -- ERROR:
   The_File_Mode := Dir_IO.Append_File;                             -- ERROR:
   
   Illegal_Even_With_Use_Clause:
   declare
      use Dir_IO;
   begin
      Create (Data_File, Append_File, Filename);                    -- ERROR:
      Open   (Data_File, Append_File, Filename);                    -- ERROR:
      Reset  (Data_File, Append_File);                              -- ERROR:
      The_File_Mode := Append_File;                                 -- ERROR:
   end Illegal_Even_With_Use_Clause;

   Illegal_Across_Package_Boundary:
   declare

      package Use_Dir_IO is
         use Dir_IO;
      end Use_Dir_IO;

      package body Use_Dir_IO is
      begin
         Create (Data_File, Append_File, Filename);                 -- ERROR:
         Open   (Data_File, Dir_IO.Append_File, Filename);          -- ERROR:
         Reset  (Data_File, Append_File);                           -- ERROR:
         The_File_Mode := Append_File;                              -- ERROR:
      end Use_Dir_IO;

   begin
      null;
   end Illegal_Across_Package_Boundary;

end BXA8001;
