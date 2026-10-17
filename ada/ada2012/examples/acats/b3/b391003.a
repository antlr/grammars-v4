-- B391003.A
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
--      Check that the parent type of a record extension may not be a
--      class-wide type.
--
--         Check for the basic case.
--
--         Check for the generic case, where the parent type is the
--         class-wide type of a formal tagged private type or formal
--         private extension.
--
--         Check for the instance case, where the parent type is a formal
--         tagged private type or formal private extension, and the
--         corresponding actual type is a class-wide type. Check that this
--         rule is enforced in the visible and private part of an instance.
--
-- TEST DESCRIPTION:
--      Declare a tagged type T.
--
--      For the basic case, attempt to declare a record extension with a
--      parent type T'Class. Check in visible and private parts.
--
--      For the generic case, declare a generic package with a formal tagged
--      private type PT and a formal private extension PE, and attempt to
--      declare record extensions with parent types PT'Class and PE'Class.
--      Check in the visible and private part.
--
--      For the instance case, declare a generic package with a formal
--      tagged private type T and declare a record extension in the visible
--      part. Attempt to instantiate the generic with a class-wide actual type.
--      Declare a second generic package with a formal private extension T
--      and declare a record extension in the private part. Attempt to
--      instantiate the generic with a class-wide actual type.  Include a
--      case where the class-wide actual is passed through to a nested
--      instantiation.                                         
--
--
-- CHANGE HISTORY:
--      02 Mar 95   SAIC    Initial version.
--      16 Jun 95   SAIC    Added nested instantiation case.
--
--!

package B391003 is

   subtype Index is Natural range 1 .. 100;

   type Tagged_Type is tagged record
      C : Index := 100;
   end record;

   --
   -- Basic case: 
   --

   type New_Tagged_Type is new Tagged_Type'Class with record          -- ERROR:
      I : Integer := 10;              -- Class-wide type not allowed as parent.
   end record;
                                  

   --
   -- Generic cases: 
   --

   generic
      type F_Tag_Private is tagged private;
      type F_Private_Extension is new Tagged_Type with private;

   package Tag_Private_Formal is 
      type G_Extension 
        is new F_Private_Extension'Class with null record;            -- ERROR:
                                      -- Class-wide type not allowed as parent.

   private
      type G_Private_Extension 
        is new F_Tag_Private'Class with null record;                  -- ERROR:
   end Tag_Private_Formal;            -- Class-wide type not allowed as parent.


   --
   -- Instance cases: 
   --

   generic
      type F_Tag_Private (<>) is tagged private;
  
   package Inst_Case_Tag_Private is
      type G_Extension is new F_Tag_Private with null record;
   end Inst_Case_Tag_Private;

   generic
      type F_Pri_Extension (<>) is new Tagged_Type with private;

   package Inst_Case_Pri_Extension is
   private
      type G_Extension is new F_Pri_Extension with null record;
   end Inst_Case_Pri_Extension;

   generic
      type F_Pri_Extension2 (<>) is new Tagged_Type with private;
   package Test2 Is
      type G_Extension2 is new F_Pri_Extension2 with null record;
   end Test2;
 
   generic
      type F_Pri_Extension (<>) is new Tagged_Type with private;
   package Inst_Case_Pri_Extension3 is
   private
      package Nest is new Test2 (F_Pri_Extension); 
   end Inst_Case_Pri_Extension3;

   package I1 is new Inst_Case_Tag_Private (Tagged_Type'Class);       -- ERROR:
                  -- Class-wide type not allowed as parent of record extension.

   package I2 is new Inst_Case_Pri_Extension (Tagged_Type'Class);     -- ERROR:
                  -- Class-wide type not allowed as parent of record extension.

   package I3 is new Inst_Case_Pri_Extension3 (Tagged_Type'Class);    -- ERROR:
                   -- Class-wide type not allowed as parent of record extension
                   -- (which occurs within a nested instantiation).

private

   --
   -- Basic case: 
   --

   type New_Private_Tagged_Type 
     is new Tagged_Type'Class with null record;                       -- ERROR:
                                      -- Class-wide type not allowed as parent.
end B391003;
