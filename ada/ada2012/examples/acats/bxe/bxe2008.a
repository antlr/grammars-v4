-- BXE2008.A
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
--      Check that a declared Remote_Types library unit may not contain:
--      variable declarations; private types where the full view of the 
--      type contains a non-remote access type and no READ and WRITE 
--      attributes are supplied; visible access types where the type is
--      neither an access-to-subprogram type nor a general access type
--      that designates a class-wide limited private type.
-- 
--      Check that a declared Remote_Types library unit may contain:
--      private types where the full view of the type contains a non-remote
--      access type and READ and WRITE attributes are supplied.
--
-- TEST DESCRIPTION:
--      This test declares one of each of the items that should not be
--      allowed and one of each of the specific items that should be
--      allowed.
--
-- APPLICABILITY CRITERIA:
--      This test applies only to implementations supporting the
--      Distribution Annex.
--
--
-- CHANGE HISTORY:
--     02 MAR 95   SAIC    Initial version
--     23 OCT 95   SAIC    Added coverage for E.2.2(5)
--
--!

with Ada.Streams;
package BXE2008 is
  pragma Remote_Types (BXE2008);
 
  -----
  -- first, some legal declarations
  -----
 
  Non_Variable : constant Integer := 19;                              -- OK.

 
  -- tagged types
  type Coordinate is tagged                                           -- OK.
    record
      X, Y : Float;
    end record;

  type Good_Private_Type is private;                                  -- OK.

  type Good_Private_Type_2 is private;                                -- OK.
   
 
  -----
  -- now, some illegal declarations
  -----
 
  A_Visible_Variable : Integer;                                       -- ERROR:
                                                   -- variables are not allowed

  type Bad_Access_1 is access Character;                              -- ERROR:
             -- visible access type must be either an access-to-subprogram type
             -- or a general access type that designates a class-wide limited 
             -- private type.

  type Bad_Access_2 is access Coordinate'Class;                       -- ERROR:
                                  -- the class wide type is not limited private
    

  type Bad_Private_Type is private;                          -- OPTIONAL ERROR:
                -- the full view of this type is where the error is so it is ok
                -- for the error message to not appear here.

  type Bad_Private_Access is private;                        -- OPTIONAL ERROR:
                -- the full view of this type is where the error is so it is ok
                -- for the error message to not appear here.
 
private 
  type Non_Remote_Access_Type_with_Attributes is access Integer;      -- OK.
  -- this access type has 'READ and 'WRITE attributes

  procedure Write (
     Stream : access Ada.Streams.Root_Stream_Type'Class;
     Item   : in Non_Remote_Access_Type_with_Attributes);
  for Non_Remote_Access_Type_with_Attributes'WRITE use Write;

  procedure Read (
     Stream : access Ada.Streams.Root_Stream_Type'Class;
     Item   : out Non_Remote_Access_Type_with_Attributes);
  for Non_Remote_Access_Type_with_Attributes'READ use Read;

  type Good_Private_Type is                                           -- OK.
    record
      Component : Non_Remote_Access_Type_with_Attributes;
    end record;

  type Good_Private_Type_2 is access Boolean;                         -- OK.

  procedure Write (
     Stream : access Ada.Streams.Root_Stream_Type'Class;
     Item   : in Good_Private_Type_2);
  for Good_Private_Type_2'WRITE use Write;

  procedure Read (
     Stream : access Ada.Streams.Root_Stream_Type'Class;
     Item   : out Good_Private_Type_2);
  for Good_Private_Type_2'READ use Read;
  

  type Non_Remote_Access_Type_without_Attributes is access Integer;   -- OK.
  -- this access type does not have 'READ and 'WRITE attributes

  type Bad_Private_Type is                                            -- ERROR:
          -- This type contains a component of a non-remote access type and the
          -- access type does not have user specified READ and WRITE attributes 
    record
      Component : Non_Remote_Access_Type_without_Attributes;
    end record;

  type Bad_Private_Access is access Integer;                          -- ERROR:
             -- This type is a non-remote access type and it does not have user
             -- specified READ and WRITE attributes.

  function Non_Static_Function return Integer;                        -- OK.

  Illegal_Constant : constant Integer := Non_Static_Function;         -- ERROR:
                                          -- This construct is not preelaborable

end BXE2008;
