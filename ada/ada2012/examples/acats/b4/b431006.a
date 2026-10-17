-- B431006.A
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
--
--*
-- OBJECTIVE:
--     Check that if a variant part is not nested in an unselected variant,
--     the value of the governing discriminant of a variant in a record
--     aggregate cannot be non-static.
--
-- TEST DESCRIPTION:
--     AI05-0220-1 corrected 4.3.1(17/3) to avoid a circular definition and
--     to match existing practice. This test has been enhanced with
--     additional test cases to check that this rule is implemented as intended.
--
--     Note that the original wording 4.3.1(17) would have certainly allowed E
--     and F, and argubly would have allowed C and D.
--
-- CHANGE HISTORY:
--     16 Jun 1988  DHH  Created original test.
--     29 Dec 2014  RLB  Converted to modern format, added test cases from
--                       AI05-0220-1.


procedure B431006 is

   subtype Small is Integer range 1 .. 3;

   type Var_Rec(Dis : Small) is
      record
         Char : Character;
            case Dis is
               when 1 =>
                  Bool : Boolean;
               when 2 =>
                  T : Character;
               when 3 =>
                  I : Integer;
            end case;
       end record;

   X : Small := 2;

   subtype Non_Static_Small is Small range 1 .. X;

   Y : constant Non_Static_Small := 1;

   A : Var_Rec(1);

   B : Var_Rec(3);

   type Enum is (One, Two, Three);

   function Get_3 return Enum is
   begin
      return Three;
   end Get_3;

   Z : constant Enum := Enum'Val(Y+1); -- Non-static value of Three.

   type Var_Rec_2 (Disc : Enum) is record
      case Disc is
         when One =>
            I : Integer;
         when Two =>
            F : Float;
         when Three =>
            null;
      end case;
   end record;

   C : Var_Rec_2(Three);

   D : Var_Rec_2(Three);

   type Var_Rec_3 (Disc : Enum := One) is record
      case Disc is
         when others => null;
      end case;
   end record;

   E: Var_Rec_3;

   F: Var_Rec_3;

   type Var_Rec_4 (Disc : Enum := One; Disc2 : Boolean := True) is record
      case Disc is
         when One =>
            I : Integer;
            case Disc2 is
                when True =>
                    J : Integer;
                when False =>
                    null;
            end case;
         when Two =>
            F : Float;
         when Three =>
            null;
      end case;
   end record;

   G : Var_Rec_4;

   H : Var_Rec_4;

   I : Var_Rec_4;

   J : Var_Rec_4;

   type Var_Rec_5 (Disc : Enum) is tagged record
      case Disc is
         when One =>
            I : Integer;
         when Two =>
            C : Character;
         when Three =>
            null;
      end case;
   end record;

   type Der_Rec (D2 : Boolean) is new Var_Rec_5 (Disc => Get_3)
      with null record;

   K : Der_Rec(False);

   type Der_Rec_2 (D2 : Boolean) is new Var_Rec_5 (Disc => Enum'Pred(Z))
      with null record;

   L : Der_Rec_2(False);

   type Der_Rec_3 (D2 : Boolean) is new Var_Rec_5 (Disc => One)
      with null record;

   M : Der_Rec_3(False);

begin

   A := (Y,                                      -- ERROR: Not static.
         'T',
         True);

   B := (3 * Non_Static_Small'First,             -- ERROR: Not static.
        'T',
        7);

   C := (Disc => Get_3);                         -- ERROR: Not static.

   D := (Disc => Z);                             -- ERROR: Not static.

   E := (Disc => Get_3);                         -- ERROR: Not static.

   F := (Disc => Z);                             -- ERROR: Not static.

   G := (Disc => Three, Disc2 => Z = Two);       -- OK.

   H := (Disc => One,
         I => 1,
         Disc2 => Z /= Three);                   -- ERROR: Not static.

   I := (Disc2 => False,
         Disc  => Get_3);                        -- ERROR: Not static.

   J := (Disc  => Z,                             -- ERROR: Not static.
         Disc2 => True);

   K := (D2 => False);                           -- ERROR: Non-static.
      -- (The discriminant that governs the variant part of Var_Rec_5 in the
      -- derived type Der_Rec is non-static).

   L := (D2 => False, C => 'A');                 -- ERROR: Non-static.
      -- (The discriminant that governs the variant part of Var_Rec_5 in the
      -- derived type Der_Rec_2 is non-static).

   M := (D2 => Z /= Three, I => 10);             -- OK.

end B431006;
