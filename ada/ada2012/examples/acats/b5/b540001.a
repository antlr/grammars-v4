-- B540001.A
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
--     Check that if the selecting expression of a case statement is a
--     name with a static nominal subtype that has a static predicate, then
--     no discrete_choice can cover any value that does not satisfy its
--     predicates.
--
--     Check that if the selecting expression of a case statement is a
--     name with a static nominal subtype that has a static predicate, and
--     there is no others choice, then every value that satisfies the
--     predicates of the subtype must be covered by some discrete_choice.
--
--     Check that a case statement choice is illegal if it is a subtype
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
--
-- CHANGE HISTORY:
--      14 Jan 12   RAD     Initial version.
--      23 May 14   RLB     Split case statement test into separate
--                          test; added extra test cases.
--      08 Feb 18   RLB     Added error location indicators to reflect common
--                          error reporting strategies.
--
--!

procedure B540001 is

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
      with Static_Predicate => Small_Even in 0 | 2 | 4 | 6 | 8 | 10 |
                                             12 | 14 | 16 | 18 | 20;
   subtype Small_Odd is Small
      with Static_Predicate => Small_Odd in 1 | 3 | 5 | 7 | 9 | 11 | 13 |
                                           15 | 17 | 19;
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


   procedure Full_Coverage_1 (X : Integer; Y : Colors; Z : Score_Base) is
      -- Check 3.8.1(10.1/4) using 5.4(9/3) and 5.4(10).
   begin
      case X is
         when Zero => null;
         when Non_Zero => null;
      end case;                          -- OK. {3:7;1}
      case X is
         when Non_Zero => null;
      end case;                          -- ERROR: {2:7;1} Missing 0.
      case X is
         when Nil => null;
         when Non_Nil => null;
      end case;                          -- OK. {3:7;1}
      case X is
         when Nil => null;
         when Positive => null;
      end case;                          -- ERROR: {3:7;1} Missing negatives.
      case X is
         when Non_Zero => null;
         when 0 | 1 => null;
      end case;                          -- ERROR: {3:7;1} 1 specified twice.

      case Y is
         when Total_Color => null;
         when Rainbow => null;
      end case;                          -- OK. {3:7;1}
      case Y is
         when Primary_Color => null;
         when Secondary_Color => null;
         when White => null;
      end case;                          -- ERROR: {4:7;1} Missing Black.
      case Y is
         when Primary => null;
         when Rainbow => null;
         when Total_Color => null;
      end case;                          -- ERROR: {4:7;1} Red, Yellow, Blue
                                         --                specified twice.
      case Y is
         when Red | Yellow => null;
         when Secondary => null;
         when Black => null;
         when White => null;
      end case;                          -- ERROR: {5:7;1} Missing Blue.

      case Z is
         when Single => null;
         when Double => null;
         when Treble => null;
         when others => null;
      end case;                          -- ERROR: {5:7;1} Many overlapping
                                         --                scores.
      case Z is
         when Score => null;
         when 0 | 23 | 29 | 31 | 35 | 37 | 41 | 43 | 44 | 46 | 47 | 49 |
              52 | 53 | 55 | 56 | 58 | 59 | 61 | 62 | 63  => null;
      end case;                          -- OK. {4:7;1}
      case Z is
         when Single => null;
         when Double_Only => null;
         when Treble_Only => null;
         when 25 | 50 => null;
         when 0 | 61 .. 63 => null;
      end case;                          -- ERROR: {6:7;1} Primes over 20
                                         --                missing.
   end Full_Coverage_1;


   procedure Full_Coverage_2 (X : Small_Even;
                              Y : Rainbow;
                              Z : Treble) is
      -- Check 5.4(7/4), all of the values satisfied by the predicates of
      -- the case selecting expression must be covered.
   begin
      case X is
         when 0 | 2 | 4 => null;
         when 6 | 8 | 10 | 12 | 14 | 16 => null;
         when 18 | 20 => null;
      end case;                          -- OK. {4:7;1}
      case X is
         when Small_Power_of_Two => null;
         when 10 | 12 | 14 | 18 => null;
      end case;                          -- ERROR: {3:7;1} 0, 6, and 20 missing

      case Y is
         when Orange => null;
         when Green => null;
         when Violet => null;
         when Primary_Color => null;
      end case;                          -- ERROR: {5:7;1} Indigo missing.
      case Y is
         when Secondary_Color => null;
         when Red => null;
         when Yellow => null;
      end case;                          -- ERROR: {4:7;1} Blue missing.
      case Y is
         when Red .. Yellow => null;
         when Blue .. Violet => null;
      end case;                          -- ERROR: {3:7;1} Green missing.

      case Z is
         when Treble_Only => null;
         when 3 | 6 | 9 | 12 | 15 | 18 => null;
         when 24 | 30 | 36 => null;
      end case;                          -- OK. {4:7;1}
      case Z is
         when 3 | 6 | 9 => null;
         when Treble_Only => null;
         when 12 | 15 | 18 => null;
      end case;                          -- ERROR: {4:7;1} 24, 30, 36 missing.
      case Z is
         when 3 | 6 | 9 | 12 | 15 | 18 => null;
         when 24 | 30 | 36 => null;
         when 21 | 27 | 33 | 39 => null;
      end case;                          -- ERROR: {4:7;1} Values over 40
                                         --                missing.
   end Full_Coverage_2;


   procedure No_Extra_Values (X : Small_Power_of_Two;
                              Y : Primary_Color;
                              Z : Double) is
      -- Check 5.4(7/4), no extra values outside of the subtype of the
      -- the case selecting expression.
   begin
      case X is
         when 2 | 4 => null;
         when 8 | 16 => null;
      end case;                          -- OK. {3:7;1}
      case X is
         when 2 | 4 | 6 => null;         -- ERROR: {1:7;9} 6 not a power of two
         when 8 | 16 => null;
      end case;
      case X is
         when 2..4 => null;              -- ERROR: {1:7;9} 3 not a power of two
         when 8 | 16 => null;
      end case;
      case X is
         when Small_Even => null;        -- ERROR: {1:7;9} Many not allowed
         when others => null;            --                values.
      end case;
      case X is
         when Small_Power_of_Two => null;-- OK. {1:7;9}
         when others => null;
      end case;

      case Y is
         when Yellow => null;
         when Blue => null;
         when Red => null;
      end case;                          -- OK. {4:7;1}
      case Y is
         when Yellow => null;
         when Blue => null;
         when Red => null;
         when Green => null;             -- ERROR: {4:7;9} Not a primary color.
         when others => null;
      end case;
      case Y is
         when Primary => null;           -- OK. {1:7;9}
      end case;
      case Y is
         when Red .. Yellow => null;     -- ERROR: {1:7;9} Orange not a primary
         when Blue => null;              --                color.
      end case;
      case Y is
         when Red | Yellow .. Blue => null; -- ERROR: {1:7;9} Green not a
         when others => null;               --                primary color.
      end case;
      case Y is
         when Yellow => null;
         when Blue => null;
         when Red => null;
         when Secondary_Color => null;   -- ERROR: {4:7;9} Not primary colors.
         when Total_Color => null;       -- ERROR: {10;9} Not primary colors.
      end case;

      case Z is
         when Single => null;            -- ERROR: {1:7;9} Odd values included.
         when Double_Only => null;       -- OK. {10;9}
         when others => null;
      end case;
      case Z is
         when 6 | 9 | 12 | 15 | 18 => null;-- ERROR: {1:7;9} 9 and 15 not
         when 2 | 4 => null;               --                doubles.
         when others => null;
      end case;
      case Z is
         when 2 .. 40 => null;           -- ERROR: {1:7;9} Odd values not
         when others => null;            --                doubles.
      end case;
      case Z is
         when 2 | 4 | 8 | 10 | 14 | 16 | 20 | 22 | 26 | 28 => null;
         when Treble => null;            -- ERROR: {2:7;9} Odd values not
         when others => null;            --                doubles.
      end case;
   end No_Extra_Values;


   procedure Non_Static (X : Integer; Y : Colors; Z : Score_Base) is
      -- Check 5.4(5/3) for the case of subtypes that have a dynamic predicate.
   begin
      case X is
         when Even => null;                 -- ERROR: {1:7;9} Not static.
         when others => null;
      end case;
      case Small'(X) is
         when Dyn_Small_Even => null;       -- ERROR: {1:7;9} Not static.
         when Small_Odd     => null;
      end case;
      case Small'(X) is
         when Small_Even_Still_Dyn => null; -- ERROR: {1:7;9} Not static.
         when Small_Odd     => null;
      end case;

      case Y is
         when Total_Color => null;
         when Dyn_Rainbow => null;          -- ERROR: {2:7;9} Not static.
      end case;
      case Y is
         when Total_Color => null;
         when Dyn_Primary => null;          -- ERROR: {2:7;9} Not static.
         when Secondary   => null;
      end case;

      case Z is
         when Dyn_Treble | 0 | 25 | 50 => null; -- ERROR: {1:7;9} Not static.
         when others => null;
      end case;
      case Z is
         when Dyn_Treble_and_Single => null; -- ERROR: {1:7;9} Not static.
         when Double_Only => null;
         when others => null;
      end case;
   end Non_Static;

begin
   Full_Coverage_1 (2, Red, 50);

   Full_Coverage_2 (6, Green, 33);

   No_Extra_Values (8, Yellow, 14);

   Non_Static (3, Blue, 25);
end B540001;
