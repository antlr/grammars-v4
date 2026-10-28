-- BXAC003.A
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
--      Check that an attempt to use the 'Write or 'Read type attribute values
--      to write or read a Stream_IO file is rejected when a stream file 
--      object is provided as the parameter, rather than an stream access 
--      value.
--      
--      Check that the correct type 'Write or 'Read attribute value is 
--      required when writing or reading data to/from a stream.
--      
--      Check that an attempt to use the 'Write or 'Read type attribute values
--      as attributes of an object rather than a type are rejected by the
--      compiler.
--      
-- TEST DESCRIPTION:
--      The test has: statements that assure a stream (rather than a
--      stream access value) can be used with 'Read and 'Write;
--      statements that use the wrong data type with 'Read and 'Write;
--      statements that assure the correct type attribute is used
--      with objects of the specific type; statements that use the wrong
--      type attributes with objects of specific types; statements that 
--      assure that the 'Read and 'Write attributes are attributes of 
--      types rather than objects of those types.
--      
--       
-- CHANGE HISTORY:
--      06 Dec 94   SAIC    ACVC 2.0
--      19 Dec 94   SAIC    Corrected comment formatting
--
--!

with Ada.Streams.Stream_IO;

procedure BXAC003 is

   type ID_Type   is range 1 .. 100;
   type Size_Type is (Small, Medium, Large, XLarge);

   type Header_Type is record
      Number_of_Elements : Natural := 0;
      Origination_Date   : String (1 .. 6);
   end record;

   type Data_Type is record
      ID   : ID_Type;
      Size : Size_Type;
   end record;

   Header : Header_Type := (20, "261093");
   Data   : Data_Type   := (67, XLarge);


   package Strm_IO renames Ada.Streams.Stream_IO;

   Stream_File_Object   : Strm_IO.File_Type;
   Stream_Access_Value  : Strm_IO.Stream_Access;


begin

   Strm_IO.Create (Stream_File_Object, 
                   Strm_IO.Out_File, 
                   "AFile");

   Stream_Access_Value := Strm_IO.Stream (Stream_File_Object);      



   -- Ensure that writing to a stream is accomplished through the use of
   -- a stream access value, rather than a stream file object.

   Header_Type'Write (Stream_File_Object,  Header);                 -- ERROR:
                                                -- Not a stream access value.
   Header_Type'Write (Stream_Access_Value, Header);                 -- OK.

   Data_Type'Write (Stream_File_Object,  Data);                     -- ERROR:
                                                -- Not a stream access value.
   Data_Type'Write (Stream_Access_Value, Data);                     -- OK.



   -- Ensure that reading from a stream is accomplished through the use of
   -- a stream access value.

   Header_Type'Read (Stream_File_Object,  Header);                  -- ERROR:
                                                -- Not a stream access value.
   Header_Type'Read (Stream_Access_Value, Header);                  -- OK.

   Data_Type'Read (Stream_File_Object,  Data);                      -- ERROR:
                                                -- Not a stream access value.
   Data_Type'Read (Stream_Access_Value, Data);                      -- OK.



   -- Ensure that the correct type attribute is used to write a specific 
   -- type value to a stream.

   Header_Type'Write (Stream_Access_Value, Data);                   -- ERROR:
                                         -- Wrong data type used with 'Write.
   Header_Type'Write (Stream_Access_Value, Header);                 -- OK.

   Data_Type'Write (Stream_Access_Value, Header);                   -- ERROR:
                                         -- Wrong data type used with 'Write.
   Data_Type'Write (Stream_Access_Value, Data);                     -- OK.



   -- Ensure that the correct type attribute is used to read a specific type 
   -- value from a stream.

   Header_Type'Read (Stream_Access_Value, Data);                    -- ERROR:
                                          -- Wrong data type used with 'Read.
   Header_Type'Read (Stream_Access_Value, Header);                  -- OK.

   Data_Type'Read (Stream_Access_Value, Header);                    -- ERROR:
                                          -- Wrong data type used with 'Read.
   Data_Type'Read (Stream_Access_Value, Data);                      -- OK.



   -- Ensure that an attempt to use the 'Write or 'Read type attribute values
   -- as attributes of an object rather than a type are rejected by the
   -- compiler.

   Header'Write (Stream_Access_Value, Header);                      -- ERROR:
                           -- Attribute 'Write used here on object, not type.
   Header'Read  (Stream_Access_Value, Header);                      -- ERROR:
                            -- Attribute 'Read used here on object, not type.

   Data'Write (Stream_Access_Value, Data);                          -- ERROR:
                           -- Attribute 'Write used here on object, not type.
   Data'Read  (Stream_Access_Value, Data);                          -- ERROR:
                            -- Attribute 'Read used here on object, not type.


end BXAC003;
