-- BXAC001.A
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
--      Check that a stream is limited and may not be the target of an
--      assignment.
--
-- TEST DESCRIPTION:
--      This test ensures that the type File_Type in package 
--      Streams.Stream_IO is a limited private type.
--      The test attempts to perform an assignment operation on a stream file
--      object, as well as attempting to use non-existent predefined checks 
--      for equality and inequality.  These attempts should raise compile-time
--      errors.
--      
--      A user may mistakenly consider a stream file similar to a Direct_IO
--      file, attempting inout operations on the stream file. This test checks
--      that Append_File mode has, and that Inout_File mode has not, been added 
--      to Stream_IO.  
--      This test attempts to use Inout_File mode in an assignment, as well as
--      in various stream operations.  Each attempt should generate a compile
--      time error.  Attempts using Append_File mode should be accepted.
--      
--      
-- APPLICABILITY CRITERIA: 
--      Applicable to all implementations that support Stream_IO operations.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--
--!

with Ada.Streams.Stream_IO;

procedure BXAC001 is

   package Strm_IO renames Ada.Streams.Stream_IO;

begin

   Check_Limited_Type:
   declare
      Stream_File1   : Strm_IO.File_Type;
      Stream_File2   : Strm_IO.File_Type;
      TC_Files_Equal : Boolean;
   begin
      -- For a limited private type, assignment is not allowed, and
      -- comparisons for equality and inequality are not implicitly declared.
      -- Attempts to use these features should be compile time errors.

      Stream_File1   := Stream_File2;                               -- ERROR:
                          -- Assignment not allowed for limited private type.
      TC_Files_Equal := (Stream_File1 = Stream_File2);              -- ERROR:
                   -- Equality operator not defined for limited private type.
      TC_Files_Equal := (Stream_File1 /= Stream_File2);             -- ERROR:
                 -- Inequality operator not defined for limited private type.

   end Check_Limited_Type;


   Check_Stream_Modes:
   declare
      Stream_File   : Strm_IO.File_Type;
      The_File_Mode : Strm_IO.File_Mode;
      Filename      : constant String := "AFile";
   begin

      The_File_Mode := Strm_IO.In_File;                             -- OK.
      The_File_Mode := Strm_IO.Out_File;                            -- OK.
      The_File_Mode := Strm_IO.Append_File;                         -- OK.
      The_File_Mode := Strm_IO.Inout_File;                          -- ERROR:
                                              -- Inout mode not in Stream_IO.

      Strm_IO.Create (Stream_File, Strm_IO.Out_File, Filename);     -- OK.
      Strm_IO.Create (Stream_File, Strm_IO.Append_File);            -- OK.
      Strm_IO.Create (Stream_File, Strm_IO.Inout_File, Filename);   -- ERROR:
                                              -- Inout mode not in Stream_IO.


      Check_With_A_Use_Clause:
      declare
         use Strm_IO;
      begin

         Open (Stream_File, In_File,    Filename);                  -- OK.
         Open (Stream_File, InOut_File, Filename);                  -- ERROR:
                                              -- Inout mode not in Stream_IO.
  
         Reset (Stream_File, Append_File);                          -- OK.
         Reset (Stream_File, Inout_File);                           -- ERROR:
                                              -- Inout mode not in Stream_IO.

      end Check_With_A_Use_Clause;

   end Check_Stream_Modes;

end BXAC001;
