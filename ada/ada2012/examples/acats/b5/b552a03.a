--  B552A03.A
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
--  OBJECTIVE:
--     Check that the loop parameter of a generalized iterator cannot be
--     assigned.
--
--     Check that the loop parameter of an array component iterator cannot be
--     assigned if the array object is a constant view.
--
--     Check that the loop parameter of a container element iterator cannot be
--     assigned if the container object is a constant view.
--
--     Check that the loop parameter of a container element iterator cannot be
--     assigned if the container type does not have Variable_Indexing.
--
--  CHANGE HISTORY:
--     02 Jun 2014 RLB Created test using previously created foundation.
--     14 Jul 2014 RLB Fixed missing "constant" on Constant_Small_Array_Object.
--!

with F552A00_Sparse_Arrays;
with F552A00_Bingo_Balls;

procedure B552A03 is
   package Sparse_Integer_Arrays is new
     F552A00_Sparse_Arrays (Sparse_Array_Index => Natural,
                    Element_Type => Integer);

   Sparse_Data : Sparse_Integer_Arrays.Sparse_Array
     (Max_Elements => 10);

   Constant_Sparse_Data : constant Sparse_Integer_Arrays.Sparse_Array
     := Sparse_Data;

   Bingo_Game : F552A00_Bingo_Balls.Bingo_Game;

   subtype Small is Natural range 0 .. 10;

   type Small_Array is array (Small) of Natural;

   Small_Array_Object : Small_Array := (others => 52);

   Constant_Small_Array_Object : constant Small_Array := Small_Array_Object;

begin
   declare
      Temp, Last : Sparse_Integer_Arrays.Cursor;
   begin
      for Item in Sparse_Data.Iterate loop
         Temp := Item;                                       -- OK.
         Item := Last;                                       -- ERROR:
         Last := Temp;                                       -- OK.
      end loop;
   end;


   for Item of Small_Array_Object loop
      Item := 0;                                             -- OK.
   end loop;

   for Item of Constant_Small_Array_Object loop
      Item := 0;                                             -- ERROR:
   end loop;

   declare
      procedure Clear (Arr : in Small_Array) is
      begin
         for Item of Arr loop
            Item := 0;                                       -- ERROR:
         end loop;
      end Clear;
   begin
      Clear (Small_Array_Object);
   end;


   for Item of Sparse_Data loop
      Item := 0;                                             -- OK.
   end loop;

   for Item of Constant_Sparse_Data loop
      Item := 0;                                             -- ERROR:
   end loop;


   for Item of Bingo_Game loop
      Item := F552A00_Bingo_Balls.B_15;                      -- ERROR:
   end loop;

end B552A03;
