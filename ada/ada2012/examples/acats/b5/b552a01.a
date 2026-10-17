--  B552A01.A
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
--     Check that "reverse" cannot be used in a generalized iterator if the
--     type of the iterator_name is not a reversible iterator type.
--
--     Check that "reverse" cannot be used in a container element iterator if
--     the default iterator type of the type of the iterable_name is not a
--     reversible iterator type.
--
--  CHANGE HISTORY:
--     08 Oct 2013 BM  Initial Version.
--     31 May 2014 RLB ACATS 4.0 version: renamed foundation and test packages.
--     08 Feb 2018 RLB Added error location indicators to reflect common
--                     error reporting strategies. Fixed an overlong line.
--!
with F552A00_Prime_Numbers;
with F552A00_Sparse_Arrays;
with F552A00_Bingo_Balls;

procedure B552A01 is
   package Sparse_Integer_Arrays is new
     F552A00_Sparse_Arrays (Sparse_Array_Index => Natural,
                    Element_Type => Integer);

   Sparse_Data : aliased Sparse_Integer_Arrays.Sparse_Array
     (Max_Elements => 10);

   Primes : F552A00_Prime_Numbers.Prime_Number_Set (Max_Value => 15);

   Bingo_Game : F552A00_Bingo_Balls.Bingo_Game;

begin
   for Item of Sparse_Data loop
      Item := Item + 1;                                      -- OK. {1:4}
   end loop;

   for Item of reverse Sparse_Data loop
      Item := Item + 1;                                      -- OK. {1:4}
   end loop;

   --  A set of multiple prime numbers
   for Item in F552A00_Prime_Numbers.Iterate (Primes) loop   -- OK. {4}
      null;
   end loop;

   for Item of Bingo_Game loop                               -- OK. {4}
      null;
   end loop;

   --  A set of multiple prime numbers
   for Item in reverse
       F552A00_Prime_Numbers.Iterate (Primes) loop           -- ERROR: {1:4}
      null;
   end loop;

   for Item of reverse Bingo_Game loop                       -- ERROR: {4}
      null;
   end loop;

end B552A01;
