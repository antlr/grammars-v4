-- B457001.A
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
--
-- OBJECTIVE:
--     Check that if the selecting expression of a case expression is a
--     name with a static nominal subtype that has a static predicate, then
--     no discrete_choice can cover any value that does not satisfy its
--     predicates.
--
--     Check that if the selecting expression of a case expression is a
--     name with a static nominal subtype that has a static predicate, and
--     there is no others choice, then every value that satisfies the
--     predicates of the subtype must be covered by some discrete_choice.
--
--     Check that a case expression choice is illegal if it is a subtype
--     with a dynamic predicate.
--
-- TEST DESCRIPTION:
--     In addition to testing the objectives above, we also check that
--     a choice naming a subtype only covers the values that satisfy the
--     predicates of the subtype (3.8.1(10.1/4)).
--
--     We try examples of each kind of discrete type. Note that we do not
--     try generic formal types here, as the predicates are not visible
--     in the generic unit, and the rules are not rechecked in the instance.
--
--     For most case expression errors, we allow the error to be reported
--     anywhere in the case expression, in order to allow the maximum
--     flexibility for error handling. (Locating errors at the start, end, or
--     on a particular label all may make sense for a particular
--     implementation; it is not the job of the ACATS to emphasize one location
--     over another.) The ERROR tag is found at the end of the case expression
--     in this case, but the location indicator allows the entire case
--     expression.
--
-- CHANGE HISTORY:
--      23 May 2014   RLB   Created test from case statement version.
--      13 Dec 2017   RLB   Corrected overlong lines and added location
--                          indicators.
--
--!

procedure B457001 is

   Named_Number : constant := 1;

   type Colors is (White, Red, Orange, Yellow, Green, Blue,
                   Indigo, Violet, Black);

   subtype Zero is Integer
      with Static_Predicate => Zero = 0;
   subtype Nil is Zero;
   subtype Non_Zero is Integer
      with Static_Predicate => Non_Zero /= 0;
   subtype Non_Nil is Non_Zero;
   subtype Small is Integer range 0 .. 20;
   subtype Small_Even is Small
      with Static_Predicate => Small_Even in
                               0 | 2 | 4 | 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20;
   subtype Small_Odd is Small
      with Static_Predicate => Small_Odd in
                               1 | 3 | 5 | 7 | 9 | 11 | 13 | 15 | 17 | 19;
   subtype Small_Power_of_Two is Small
      with Static_Predicate => Small_Power_of_Two in 2 | 4 | 8 | 16;
   subtype Small_Power_of_Three is Small
      with Static_Predicate => Small_Power_of_Three in 3 | 9;
   subtype Even is Integer
      with Dynamic_Predicate => Even mod 2 = 0;
   subtype Dyn_Small_Even is Even range 0 .. 20;
   subtype Small_Even_Still_Dyn is Even
      with Static_Predicate => Small_Even_Still_Dyn in 0 .. 20;

   subtype Total_Color is Colors
      with Static_Predicate => Total_Color in White | Black;
   subtype Primary_Color is Colors
      with Static_Predicate => Primary_Color in Red | Yellow | Blue;
   subtype Secondary_Color is Colors
      with Static_Predicate =>
           Secondary_Color in Orange | Green | Indigo | Violet;
   subtype Rainbow is Colors
      with Static_Predicate => Rainbow in Red .. Violet;
   subtype Primary is Primary_Color;
   subtype Secondary is Secondary_Color;
   subtype Dyn_Rainbow is Colors
      with Dynamic_Predicate => Dyn_Rainbow in Red .. Violet;
   subtype Dyn_Primary is Dyn_Rainbow
      with Static_Predicate => Dyn_Primary in Red | Yellow | Blue;

   type Score_Base is mod 2**6;
   -- Darts scoring, borrowed from the Ada 2012 Rationale:
   subtype Single is Score_Base range 1 .. 20;
   subtype Double is Score_Base
      with Static_Predicate =>
         Double in 2 | 4 | 6 | 8 | 10 | 12 | 14 | 16 | 18 | 20 |
                   22 | 24 | 26 | 28 | 30 | 32 | 34 | 36 | 38 | 40;
   subtype Treble is Score_Base
      with Static_Predicate =>
         Treble in 3 | 6 | 9 | 12 | 15 | 18 | 21 | 24 | 27 | 30 |
                   33 | 36 | 39 | 42 | 45 | 48 | 51 | 54 | 57 | 60;
   subtype Score is Score_Base
      with Static_Predicate =>
         Score in Single | Double | Treble | 25 | 50;
   subtype Double_Only is Score_Base
      with Static_Predicate =>
         Double_Only in 22 | 24 | 26 | 28 | 30 | 32 | 34 | 36 | 38 | 40;
   subtype Treble_Only is Score_Base
      with Static_Predicate =>
         Treble_Only in 21 | 27 | 33 | 39 | 42 | 45 | 48 | 51 | 54 | 57 | 60;
   subtype Dyn_Not_Score is Score_Base
      with Dynamic_Predicate => Dyn_Not_Score not in Score;
   subtype Dyn_Treble is Score_Base
      with Dynamic_Predicate => Dyn_Treble mod 3 = 0 and then
                                Dyn_Treble in 3 .. 60;
   subtype Dyn_Treble_and_Single is Dyn_Treble
      with Static_Predicate => Dyn_Treble_and_Single in 1 .. 20;

   procedure Sink (Expr : in Boolean) is
   begin
      if Expr then
         null; -- Log here.
      end if;
   end Sink;


   procedure Full_Coverage_1 (X : Integer; Y : Colors; Z : Score_Base) is
      -- Check 3.8.1(10.1/4) using 5.4(9/3) and 5.4(10).
   begin
      Sink (case X is
              when Zero => False,
              when Non_Zero => True);            -- OK. {2:12;1}
      Sink (case X is
             when Non_Zero => False);            -- ERROR: Missing 0. {1:12;1}
      Sink (case X is
             when Nil => False,
             when Non_Nil => True);              -- OK. {2:12;1}
      Sink (case X is
             when Nil => False,
             when Positive => True);             -- ERROR: Missing  {2:12;1}
                                                 --        negatives.
      Sink (case X is
             when Non_Zero => False,
             when 0 | 1 => True);                -- ERROR: 1 specified {2:12;1}
                                                 --        twice.
      Sink (case Y is
             when Total_Color => False,
             when Rainbow => True);              -- OK. {2:12;1}
      Sink (case Y is
             when Primary_Color => True,
             when Secondary_Color => False,
             when White => False);               -- ERROR: Missing  {3:12;1}
                                                 --        Black.
      Sink (case Y is
             when Primary => False,
             when Rainbow => True,
             when Total_Color => False);         -- ERROR: Red, Yellow,{3:12;1}
                                                 --       Blue specified twice.
      Sink (case Y is
             when Red | Yellow => False,
             when Secondary => True,
             when Black => False,
             when White => False);               -- ERROR: Missing  {4:12;1}
                                                 --        Blue.
      Sink (case Z is
             when Single => False,
             when Double => False,
             when Treble => False,
             when others => True);               -- ERROR: Many   {4:12;1}
                                                 --        overlapping scores.
      Sink (case Z is
             when Score => False,
             when 0 | 23 | 29 | 31 | 35 | 37 | 41 | 43 | 44 |
                  46 | 47 | 49 | 52 | 53 | 55 | 56 | 58 | 59 |
                  61 | 62 | 63  => True);        -- OK.  {4:12;1}
      Sink (case Z is
             when Single => False,
             when Double_Only => False,
             when Treble_Only => False,
             when 25 | 50 => True,
             when 0 | 61 .. 63 => False);        -- ERROR: Primes over {5:12;1}
                                                 --        20 missing.
   end Full_Coverage_1;


   procedure Full_Coverage_2 (X : Small_Even;
                              Y : Rainbow;
                              Z : Treble) is
      -- Check 5.4(7/4), all of the values satisfied by the predicates of
      -- the case selecting expression must be covered.
   begin
      Sink (case X is
             when 0 | 2 | 4 => False,
             when 6 | 8 | 10 | 12 | 14 | 16 => False,
             when 18 | 20 => True);              -- OK. {3:12;1}
      Sink (case X is
             when Small_Power_of_Two => False,
             when 10 | 12 | 14 | 18 => True);    -- ERROR: 0, 6, and  {2:12;1}
                                                 -- 20 missing.

      Sink (case Y is
             when Orange => False,
             when Green => False,
             when Violet => False,
             when Primary_Color => True);        -- ERROR: Indigo  {4:12;1}
                                                 --        missing.
      Sink (case Y is
             when Secondary_Color => False,
             when Red => True,
             when Yellow => True);               -- ERROR: Blue  {3:12;1}
                                                 --        missing.
      Sink (case Y is
             when Red .. Yellow => False,
             when Blue .. Violet => True);       -- ERROR: Green  {2:12;1}
                                                 --        missing.
      Sink (case Z is
             when Treble_Only => True,
             when 3 | 6 | 9 | 12 | 15 | 18 => False,
             when 24 | 30 | 36 => False);        -- OK. {3:12;1}
      Sink (case Z is
             when 3 | 6 | 9 => False,
             when Treble_Only => True,
             when 12 | 15 | 18 => False);        -- ERROR: 24, 30, 36  {3:12;1}
                                                 --        missing.
      Sink (case Z is
             when 3 | 6 | 9 | 12 | 15 | 18 => False,
             when 24 | 30 | 36 => False,
             when 21 | 27 | 33 | 39 => True);    -- ERROR: Values   {3:12;1}
                                                 --        over 40 missing.
   end Full_Coverage_2;


   procedure No_Extra_Values (X : Small_Power_of_Two;
                              Y : Primary_Color;
                              Z : Double) is
      -- Check 5.4(7/4), no extra values outside of the subtype of the
      -- the case selecting expression.
   begin
      Sink (case X is
             when 2 | 4 => False,
             when 8 | 16 => True);               -- OK. {2:12;1}
      Sink (case X is
             when 2 | 4 | 6 => True,             -- ERROR: 6 not a  {19;9}
             when 8 | 16 => False);              --        power of two.
      Sink (case X is
             when 2..4 => True,                  -- ERROR: 3 not a  {19;9}
             when 8 | 16 => False);              --        power of two.
      Sink (case X is
             when Small_Even => False,           -- ERROR: Many not  {19;10}
             when others => True);               --        allowed values.
      Sink (case X is
             when Small_Power_of_Two => True,    -- OK.  {19;9}
             when others => False);

      Sink (case Y is
             when Yellow => True,
             when Blue => False,
             when Red => False);                 -- OK. {3:12;1}
      Sink (case Y is
             when Yellow => False,
             when Blue => False,
             when Red => False,
             when Green => True,                 -- ERROR: Not a  {19;9}
             when others => False);              --        primary_color.
      Sink (case Y is
             when Primary => False);             -- OK. {1:12;1}
      Sink (case Y is
             when Red .. Yellow => False,        -- ERROR: Orange not  {19;10}
             when Blue => False);                --        a primary color.
      Sink (case Y is
             when Red | Yellow .. Blue => False, -- ERROR: Green not  {19;10}
             when others => False);              --        a primary color.
      Sink (case Y is
             when Yellow => False,
             when Blue => False,
             when Red => False,
             when Secondary_Color => True,       -- ERROR:   {19;9}
             when Total_Color => True);          -- ERROR: Not   {19;10}
                                                 --        primary colors.
      Sink (case Z is
             when Single => False,               -- ERROR: Odd values. {19;10}
             when Double_Only => False,          -- OK.  {19;10}
             when others => True);
      Sink (case Z is
             when 6 | 9 | 12 | 15 | 18 => False, -- ERROR: 9 and 15  {19;10}
             when 2 | 4 => False,                --        not doubles.
             when others => True);
      Sink (case Z is
             when 2 .. 40 => True,               -- ERROR: Odd values  {19;9}
             when others => False);              --        not doubles.
      Sink (case Z is
             when 2 | 4 | 8 | 10 | 14 | 16 | 20 | 22 | 26 | 28 => True,
             when Treble => False,               -- ERROR: Odd values  {19;10}
             when others => False);              --        not doubles.
   end No_Extra_Values;


   procedure Non_Static (X : Integer; Y : Colors; Z : Score_Base) is
      -- Check 5.4(5/3) for the case of subtypes that have a dynamic predicate.
   begin
      Sink (case X is
             when Even => True,                  -- ERROR: Not static. {19;9}
             when others => False);
      Sink (case Small'(X) is
             when Dyn_Small_Even => True,        -- ERROR: Not static. {19;9}
             when Small_Odd      => False);
      Sink (case Small'(X) is
             when Small_Even_Still_Dyn => True,  -- ERROR: Not static. {19;9}
             when Small_Odd => False);

      Sink (case Y is
             when Total_Color => False,
             when Dyn_Rainbow => True);          -- ERROR: Not static. {19;10}
      Sink (case Y is
             when Total_Color => False,
             when Dyn_Primary => True,           -- ERROR: Not static. {19;9}
             when Secondary   => False);

      Sink (case Z is
             when Dyn_Treble |
                  0 | 25 | 50 => False,          -- ERROR: Not static.{1:19;10}
             when others => True);
      Sink (case Z is
             when Dyn_Treble_and_Single => True, -- ERROR: Not static. {19;9}
             when Double_Only => False,
             when others => False);
   end Non_Static;

begin
   Full_Coverage_1 (2, Red, 50);

   Full_Coverage_2 (6, Green, 33);

   No_Extra_Values (8, Yellow, 14);

   Non_Static (3, Blue, 25);
end B457001;
