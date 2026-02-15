-- B540002.A
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
--     Check that case statement coverage rules are enforced for case
--     statements of modular types.
--
-- TEST DESCRIPTION:
--     We use test cases inspired by similar legacy tests for integer types.
--
--     (A) [B54A12A] When the case selecting_expression is an object having
--     a static subtype, or is a qualified expression or type conversion
--     with a static subtype, no choice may have a value outside
--     the subtype's range.
--
--     (B) [B54B04A] Even when the context indicates that a case
--     selecting_expression covers a smaller range of values than
--     permitted by its subtype, every value of the (static) subtype's range
--     must be covered by some discrete choice.
--
--     (C) [B54A02B] When the case selecting_expression is an expression
--     with a non-static subtype, every value of the type's base range
--     must be covered by some discrete choice.
--
--     (D) [B54A20A] Check that two discrete_choices of a case statement
--     may not cover the same value.
--
-- CHANGE HISTORY:
--      21 May 14   RLB     Created test from legacy tests (as described
--                          above).
--      08 Feb 18   RLB     Added error location indicators to reflect common
--                          error reporting strategies.
--
--!

procedure B540002 is
begin
   declare

      type Byte is mod 2**8;

      subtype Short is Byte range 1 .. 100;

      A : Short;
      B : constant Short := 50;
      C : Byte;

   begin
      case A is
          when 0 => null;                  -- ERROR: {11;9} (A)
          when 100 => null;                -- OK. {11;9}
          when others => null;             -- OK. {11;9}
      end case;

      case A is
          when 1 => null;                  -- OK. {11;9}
          when 101 => null;                -- ERROR: {11;9} (A)
          when others => null;             -- OK. {11;9}
      end case;

      case B is
          when 0 => null;                  -- ERROR: {11;9} (A)
          when 100 => null;                -- OK. {11;9}
          when others => null;             -- OK. {11;9}
      end case;

      case B is
          when 1 => null;                  -- OK. {11;9}
          when 101 => null;                -- ERROR: {11;9} (A)
          when others => null;             -- OK. {11;9}
      end case;

      case Short'(50) is
          when 0 => null;                  -- ERROR: {11;9} (A)
          when 100 => null;                -- OK. {11;9}
          when others => null;             -- OK. {11;9}
      end case;

      case Short'(50) is
          when 1 => null;                  -- OK. {11;9}
          when 101 => null;                -- ERROR: {11;9} (A)
          when others => null;             -- OK. {11;9}
      end case;

      case Short(C) is
          when 0 => null;                  -- ERROR: {11;9} (A)
          when 100 => null;                -- OK. {11;9}
          when others => null;             -- OK. {11;9}
      end case;

      case Short(C) is
          when 1 => null;                  -- OK. {11;9}
          when 101 => null;                -- ERROR: {11;9} (A)
          when others => null;             -- OK. {11;9}
      end case;

   end;

   declare

      type Nibble is mod 2**4;

      subtype Stat is Nibble range 1 .. 5;

      I   : Nibble range 1 .. 5  := 2;
      J   : Stat                 := 2;

   begin
      case  I  is
         when  2  =>
            case  I  is
               when  1 | 3  =>  null;
               when  2 | 4  =>  null;
            end case;                      -- ERROR: {3:13;1} (B)

         when others  =>
            null;
      end case;

      case  Stat'( 2 )  is
         when  2  =>
            case  Stat'( 2 )  is
               when  5 | 2..4  =>  null;
            end case;                      -- ERROR: {2:13;1} (B)
         when others  =>
            null;
      end case;

      case  Stat( J )  is
         when  2  =>
            case  Stat( J )  is
               when  5 | 2..3  =>  null;
               when  1         =>  null;
            end case;                      -- ERROR: {3:13;1} (B)
         when others  =>
            null;
      end case;
   end;

   declare
      type Twiddle is mod 2**6;

      TW_5  : Twiddle   :=  5;
      subtype  Stat   is  Twiddle range 1..50;
      subtype  Dyn    is  Stat    range 1..TW_5;
      I   : Stat range 1..TW_5;
      J   : Dyn;

      function FF return Dyn is
      begin
         return 2;
      end  FF;

   begin
      I  :=  2;
      J  :=  2;

      case  I  is
         when  3 | 5  =>  null;
         when  2 | 4  =>  null;
         when  0..0 | 6..Twiddle'Last  =>  null;
      end case;                            -- ERROR: {4:7;1} (C)

      case  I  is
         when  3 | 5      =>  null;
         when  2 | 4      =>  null;
         when  1 | 6..50  =>  null;
      end case;                            -- ERROR: {4:7;1} (C)

      case  J  is
         when  1 | 3 | 5  =>  null;
         when  2 | 4      =>  null;
         when  0 | 7..Twiddle'Last  =>  null;
      end case;                            -- ERROR: {4:7;1} (C)

      case  J  is
         when  1 | 3 | 5  =>  null;
         when  2 | 4      =>  null;
      end case;                            -- ERROR: {3:7;1} (C)

      case  Dyn'First  is
         when  1..Stat'Last  =>  null;
      end case;                            -- ERROR: {2:7;1} (C)

      case  FF  is
         when  4..5  =>  null;
         when  6..Twiddle'Last  =>  null;
         when  1..3  =>  null;
      end case;                            -- ERROR: {4:7;1} (C)

      case  Dyn'( 2 )  is
         when  6..Twiddle'Last  =>  null;
         when  5 | 1..4  =>  null;
      end case;                            -- ERROR: {3:7;1} (C)

      case  Dyn( J )  is
         when  5 | 2..4  =>  null;
         when  1         =>  null;
         when  6..50     =>  null;
      end case;                            -- ERROR: {4:7;1} (C)

      case  ( FF )  is
         when  1 | 5  =>  null;
         when  2 | 4  =>  null;
         when  3      =>  null;
      end case;                            -- ERROR: {4:7;1} (C)

      case  ( Dyn'( I ) )  is
         when  4..5  =>  null;
         when  1..3  =>  null;
         when  0 | 6..50  =>  null;
      end case;                            -- ERROR: {4:7;1} (C)

      case  ( Dyn( 2 ) )  is
         when  2..4  =>  null;
         when  1     =>  null;
         when  5..50 =>  null;
      end case;                            -- ERROR: {4:7;1} (C)

   end;

   declare
      type Byte is mod 2**8;
      subtype Tiny is Byte range 1 .. 10;

      I1_10     : constant Byte := 10;
   begin

      case Tiny'(5) is
         when 7 .. I1_10 => null;          -- OK. {10;9}
         when 4 .. 8 => null;              -- ERROR: {10;9} (D)
         when others => null;
      end case;

      case Tiny'(5) is
         when 7 .. 10 => null;
         when 1 .. 4 => null;
         when 5 .. 7 => null;              -- ERROR: {10;9} (D)
      end case;

      case Byte'(5) is
         when 1..4 | 10..13 | 3..5 => null;-- ERROR: {10;9} (D)
         when others => null;
      end case;

      case Tiny'(4) is
         when 1 | 3 | 7 | 5 | 9 | 8 => null;
         when 2 | 4 | 6 | 8 | 10 => null;  -- ERROR: {10;9} (D)
      end case;

   end;

end B540002;
