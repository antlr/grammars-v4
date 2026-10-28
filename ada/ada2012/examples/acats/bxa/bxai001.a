-- BXAI001.A
--
--                             Grant of Unlimited Rights
--
--     The Ada Conformity Assessment Authority (ACAA) holds unlimited
--     rights in the software and documentation contained herein. Unlimited
--     rights are the same as those granted by the U.S. Government for older
--     parts of the Ada Conformity Assessment Test Suite, and are defined
--     in DFAR 252.227-7013(a)(19). By making this public release, the ACAA
--     intends to confer upon all recipients unlimited rights equal to those
--     held by the ACAA. These rights include rights to use, duplicate,
--     release or disclose the released technical data and computer software
--     in whole or in part, in any manner and for any purpose whatsoever, and
--     to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. THE ACAA MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--
--                                     Notice
--
--     The ACAA has created and maintains the Ada Conformity Assessment Test
--     Suite for the purpose of conformity assessments conducted in accordance
--     with the International Standard ISO/IEC 18009 - Ada: Conformity
--     assessment of a language processor. This test suite should not be used
--     to make claims of conformance unless used in accordance with
--     ISO/IEC 18009 and any applicable ACAA procedures.
--*
-- OBJECTIVE:
--      Check that reverse iterators are not allowed for package
--      Ada.Containers.Hashed_Maps.
--
-- TEST DESCRIPTION:
--      This test checks that forward, but not reverse iterators in the
--      Ada 2012 syntax are supported for Hashed_Maps. We check this as
--      it is different than most of the other containers.
--
-- CHANGE HISTORY:
--      20 Nov 14   RLB     Created from parts of CXAI020.
--
with Ada.Containers.Hashed_Maps;
with Report;

procedure BXAI001 is

   My_Default_Value : constant := 999.0;

   type My_Float is new Float
     with Default_Value => My_Default_Value;

   type My_Key_Type is new Integer;

   use type Ada.Containers.Hash_Type;

   function My_Hash (Key : My_Key_Type) return Ada.Containers.Hash_Type is
   begin

      return Ada.Containers.Hash_Type (Key) * 17; -- Some prime

   end My_Hash;

   function My_Equivalent_Keys (Left, Right : My_Key_Type) return Boolean is
   begin

      return Left = Right;

   end My_Equivalent_Keys;

   package My_Hashed_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => My_Key_Type,
      Element_Type    => My_Float,
      Hash            => My_Hash,
      Equivalent_Keys => My_Equivalent_Keys); -- Default =

   My_Map_1 : My_Hashed_Maps.Map;

   Num_Tests : constant := 10;

   subtype Array_Bounds_Type is Ada.Containers.Count_Type range 1 .. Num_Tests;

   -- No fractional parts so that can compare values for equality

   Value_In_Array  : constant array (Array_Bounds_Type) of My_Float :=
     (12.0, 23.0, 34.0, 45.0, 56.0, 67.0, 78.0, 89.0, 100.0, 111.0);

   My_Cursor_1 : My_Hashed_Maps.Cursor;

   use type Ada.Containers.Count_Type;
   use type My_Hashed_Maps.Cursor;
   use type My_Hashed_Maps.Map;


begin

   Report.Test
     ("BXAI001",
      "Check that reverse iterators aren't supported for package " &
      "Ada.Containers.Hashed_Maps");

   -- Test Ada 2012 Iterate (2 forms) and (implicitly) Constant_Reference and
   -- Reference

   declare

      Total_Out : My_Float;

   begin

      My_Map_1.Clear;

      for E of My_Map_1 loop                     -- OK.

         Total_Out := Total_Out + E;

      end loop;

      for E of reverse My_Map_1 loop             -- ERROR:

         Total_Out := Total_Out + E;

      end loop;

      for C in My_Map_1.Iterate loop             -- OK.

         Total_Out := Total_Out + My_Map_1 (C);

      end loop;

      for C in reverse My_Map_1.Iterate loop     -- ERROR:

         Total_Out := Total_Out + My_Map_1 (C);

      end loop;

      for C in My_Map_1.Iterate (Start => My_Cursor_1) loop  -- ERROR:
         -- No Start parameter, either.

         Total_Out := Total_Out + My_Map_1 (C);

      end loop;

   end;

   Report.Result;

end BXAI001;
