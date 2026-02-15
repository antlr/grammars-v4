--  B552A02.A
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
--     Check that the iterator_name of a generalized iterator cannot be of a
--     non-iterator type.
--
--     Check that the iterable_name of an iterator cannot be of a type that
--     is neither an array nor an iterable container type.
--
--     Check that the iterable_name of an iterator cannot denote a type.
--
--  TEST DESCRIPTION:
--     We mainly concentrate on cases where the loop would have been legal
--     if the other syntax had been used ("of" instead of "in", or vice
--     versa).
--
--  CHANGE HISTORY:
--     02 Jun 2014 RLB Created test using previously created foundation.
--!

with F552A00_Prime_Numbers;
with F552A00_Sparse_Arrays;
with F552A00_Bingo_Balls;

procedure B552A02 is
   package Sparse_Integer_Arrays is new
     F552A00_Sparse_Arrays (Sparse_Array_Index => Natural,
                    Element_Type => Integer);

   Sparse_Data : Sparse_Integer_Arrays.Sparse_Array
     (Max_Elements => 10);

   Sparse_Cursor : Sparse_Integer_Arrays.Cursor;

   Primes : F552A00_Prime_Numbers.Prime_Number_Set (Max_Value => 15);

   Bingo_Game : F552A00_Bingo_Balls.Bingo_Game;

   subtype Small is Natural range 0 .. 10;

   Small_Object : Small;

   type Small_Array is array (Small) of Natural;

   Small_Array_Object : Small_Array;


begin
   for Item in Small loop                                    -- OK.
      null;
   end loop;

   for Item in Sparse_Data loop                              -- ERROR:
      null;
   end loop;

   for Item in Bingo_Game loop                               -- ERROR:
      null;
   end loop;

   for Item in Small_Array_Object loop                       -- ERROR:
      null;
   end loop;

   for Item in Sparse_Cursor loop                            -- ERROR:
      null;
   end loop;


   for Item of Small_Object loop                             -- ERROR:
      null;
   end loop;

   for Item of Primes.Iterate loop                           -- ERROR:
      null;
   end loop;

   for Item of Small_Array_Object loop                       -- OK.
      null;
   end loop;

   for Item of Bingo_Game loop                               -- OK.
      null;
   end loop;

   for Item of Sparse_Cursor loop                            -- ERROR:
      null;
   end loop;


   for Item of Small loop                                    -- ERROR:
      null;
   end loop;

   for Item of Small_Array loop                              -- ERROR:
      null;
   end loop;

   for Item of F552A00_Bingo_Balls.Bingo_Game loop           -- ERROR:
      null;
   end loop;

end B552A02;
