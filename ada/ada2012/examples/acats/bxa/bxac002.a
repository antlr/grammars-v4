-- BXAC002.A
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
--      Check that the Set_Position procedure and Position function are not
--      predefined in Stream_IO.  Check that the type File_Offset is not 
--      predefined in Stream_IO.  Check that the Set_Index procedure and 
--      Index function are predefined in Stream_IO.  Check that the type 
--      Positive_Count is predefined in Stream_IO.  Check that the 
--      appropriate parameter types are required for the Stream_IO procedure 
--      Set_Index.
--      
-- TEST DESCRIPTION:
--      The function Index and procedure Set_Index use stream file objects as
--      file parameters (and not access objects as for the attributes 'Read 
--      and 'Write), with additional parameters of the appropriate types
--      (i.e., not integers).
--      
-- APPLICABILITY CRITERIA: 
--      Applicable to all implementations that support Stream_IO operations.
--
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      27 Nov 95   SAIC    Corrected variable qualification for ACVC 2.0.1.
--
--!

with Ada.Streams.Stream_IO;

procedure BXAC002 is

   package Strm_IO renames Ada.Streams.Stream_IO;

   My_Stream          : Strm_IO.File_Type;
   My_Stream_Access   : Strm_IO.Stream_Access;

   Position_In_Stream : Strm_IO.Positive_Count;
   Incorrect_Position : Strm_IO.File_Offset := 0;                   -- ERROR:
                               -- File_Offset no longer defined in Stream_IO.
   Integer_Position   : Integer;
   Beginning_Of_File  : Integer;

begin

   Strm_IO.Create (My_Stream);
   My_Stream_Access := Strm_IO.Stream (My_Stream);

   -- The function Index takes a file_type object as a parameter, not
   -- a stream access object.  It returns a value of type Positive_Count.

   Position_In_Stream := Strm_IO.Index (My_Stream);                 -- OK.
   Position_In_Stream := Strm_IO.Index (My_Stream_Access);          -- ERROR:
     -- Stream access object not appropriate as parameter for function Index.
   Position_In_Stream := Strm_IO.Position (My_Stream);              -- ERROR:

   -- The procedure Set_Index takes a file object as a parameter, not
   -- a stream access object.  

   Strm_IO.Set_Index (File => My_Stream,                               -- OK.
                      To   => Position_In_Stream);

   Strm_IO.Set_Index (File => My_Stream_Access,                     -- ERROR:
                      -- File_Type object required, not stream_access object.
                      To   => Position_In_Stream);

   Strm_IO.Set_Position (File        => My_Stream,                  -- ERROR:
                         Offset      => Position_In_Stream,        
                         Relative_To => Beginning_Of_File);



   -- The procedure Set_Index does not take an offset parameter
   -- of type integer.

   Strm_IO.Set_Index (File => My_Stream,             
                      To   => Integer_Position);                    -- ERROR:
                       -- Procedure takes a parameter of type Positive_Count.

   -- The procedure Set_Index takes does not take a stream_access parameter.

   Strm_IO.Set_Index (File => My_Stream_Access,                     -- ERROR:
                        -- Stream_Access object not appropriate as parameter.
                      To   => Position_In_Stream); 
                                                              
   Strm_IO.Set_Index (File => My_Stream,                               -- OK.
                      To   => Position_In_Stream); 


                         
end BXAC002;
