-- B457004.A
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
--     ISO/IEC 18010 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--     Check that a case expression is illegal if any of the choices are
--     non-static.
--
--     Check that a case expression is illegal if any of the possible values
--     of the selecting expression are not covered by a choice.
--
--     Check that a case expression is illegal if more than one choice
--     covers the same value.
--
-- TEST DESCRIPTION:
--     This test checks cases involving objects, ranges, and subtypes. Cases
--     involving predicates are found in B457001.
--
--     For most case expression errors, we allow the error to be reported
--     anywhere in the case expression, in order to allow the maximum
--     flexibility for error handling. (Locating errors at the start, end, or
--     on a particular label all may make sense for a particular
--     implementation; it is not the job of the ACATS to emphasize one location
--     over another.) The ERROR tag is found at the end of the case expression
--     in this case, but the location indicator allows the entire case
--     expression.

-- CHANGE HISTORY:
--      02 Jan 2015   RLB   Created test from predicate version (B457001).
--      13 Mar 2015   RLB   Eliminate overlong lines.
--      12 Dec 2017   RLB   Corrected two test errors and added location
--                          indicators.
--
--!

procedure B457004 is

   Named_Number : constant := 1;

   type Colors is (White, Red, Orange, Yellow, Green, Blue,
                   Indigo, Violet, Black);

   subtype Zero is Integer range 0 .. 0;
   subtype Negative is Integer range Integer'First .. -1;
   subtype Small is Integer range 0 .. 20;
   subtype Small_Low is Small range 0 .. 10;
   subtype Small_High is Small range 11 .. 20;
   Dyn_Zero : Integer := 0;
   Dyn_Ten  : Integer := 10;
   subtype Dyn_Low is Integer range Dyn_Zero .. Dyn_Ten;
   subtype Dyn_Low_Half is Dyn_Low range 0 .. 5;

   subtype Warm_Color is Colors range Red .. Yellow;
   subtype Cool_Color is Colors range Green .. Violet;
   subtype Rainbow is Colors range Red .. Violet;
   V : Colors := Violet;
   subtype Dyn_Rainbow is Colors range Red .. V;
   subtype Dyn_Warm is Dyn_Rainbow range Red .. Yellow;

   type Score_Base is mod 2**6;
   subtype Single is Score_Base range 1 .. 20;
   subtype Double is Score_Base range 2 .. 40;
   subtype Treble is Score_Base range 3 .. 60;
   subtype Double_Only is Score_Base range 22 .. 40;
   subtype Treble_Only is Score_Base range 21 .. 60;

   Single_High : Score_Base := 20;
   Highest : Score_Base := 60;
   subtype Dyn_Not_Single is Score_Base range 21 .. Highest;

   procedure Sink (Expr : in Boolean) is
   begin
      if Expr then
         null; -- Log here.
      end if;
   end Sink;


   procedure Full_Coverage_1 (X : Integer; Y : Colors; Z : Score_Base) is
      -- Check 3.8.1(10.1/4) using 5.4(9/3) and 5.4(10) via 4.5.7(19/3).
   begin
      Sink (case X is
             when Zero => False,
             when Negative | Positive => True);  -- OK. {2:12;1}
      Sink (case X is
             when Negative => False,
             when Positive => True);             -- ERROR: Missing 0. {2:12;1}
      Sink (case X is
             when Zero => False,
             when Positive => True);             -- ERROR: Missing  {2:12;1}
                                                 --        negative values.
      Sink (case X is
             when Negative | Positive => False,
             when 0 | 1 => True);                -- ERROR: 1  {2:12;1}
                                                 --        appears twice.
      Sink (case Y is
             when White | Black => False,
             when Rainbow => True);              -- OK.  {2:12;1}
      Sink (case Y is
             when Warm_Color | Cool_Color => True,
             when White => False);               -- ERROR: Missing  {2:12;1}
                                                 --        Black.
      Sink (case Y is
             when Warm_Color => False,
             when Rainbow => True,
             when White | Black => False);       -- ERROR: Red,  {3:12;1}
                                                 --    Orange, Yellow
                                                 --    specified twice.
      Sink (case Y is
             when Red | Yellow => False,
             when Cool_Color => True,
             when Black => False,
             when White => False);               -- ERROR: Missing  {4:12;1}
                                                 --        Orange.

      Sink (case Z is
             when Single => False,
             when Double => False,
             when Treble => False,
             when others => True);               -- ERROR: Many  {4:12;1}
                                                 --        overlapping scores.
      Sink (case Z is
             when Single => False,
             when 0 | 21 .. 63 => True);         -- OK.  {2:12;1}
      Sink (case Z is
             when Single => False,
             when Double_Only => False,
             when 0 | 41 .. 63 => True);         -- ERROR: 21 missing.{3:12;1}
   end Full_Coverage_1;


   procedure Full_Coverage_2 (X : Small_High;
                              Y : Rainbow;
                              Z : Treble) is
      -- Check 5.4(7/4) via 4.5.7(19/3), all of the values of the case
      -- selecting expression must be covered.
   begin
      Sink (case X is
             when 11 | 12 | 14 | 16 => False,
             when 13 | 15 | 17 | 19 => False,
             when 18 | 20 => True);              -- OK. {3:12;1}
      Sink (case X is
             when 16 | 19 => False,
             when 11 .. 14 | 18 => True);        -- ERROR: 15, 17,  {2:12;1}
                                                 --        and 20 missing.
      Sink (case Y is
             when Blue => False,
             when Green => False,
             when Violet => False,
             when Warm_Color => True);           -- ERROR: Indigo  {4:12;1}
                                                 --        missing.
      Sink (case Y is
             when Cool_Color => False,
             when Red => True,
             when Yellow => True);               -- ERROR: Orange  {3:12;1}
                                                 --        missing.
      Sink (case Y is
             when Red .. Yellow => False,
             when Blue .. Violet => True);       -- ERROR: Green  {2:12;1}
                                                 --        missing.
      Sink (case Z is
             when Treble_Only => True,
             when 3 .. 20 => False);             -- OK.  {2:12;1}
      Sink (case Z is
             when 3 | 6 | 9 => False,
             when Treble_Only => True,
             when 10 .. 20 => False);            -- ERROR: 4, 5, 7, 8 {3:12;1}
                                                 --        missing.
      Sink (case Z is
             when 3 .. 21 => False,
             when Double_Only => True);          -- ERROR: Values   {2:12;1}
                                                 --        over 40 missing.
   end Full_Coverage_2;


   procedure No_Extra_Values (X : Small_Low;
                              Y : Warm_Color;
                              Z : Double) is
      -- Check 5.4(7/4), no extra values outside of the subtype of the
      -- the case selecting expression.
   begin
      Sink (case X is
             when 0 .. 9 => False,
             when 10 => True);                   -- OK.  {2:12;1}
      Sink (case X is
             when 0 .. 7 | 9 => True,
             when 8 | 16 => False);              -- ERROR: 16 not in  {19;11}
                                                 --        Small_Low.
      Sink (case X is
             when 0 .. 4 => True,
             when 5 .. 12 => False);             -- ERROR: 11, 12    {19;11}
                                                 --     not in Small_Low.
      Sink (case X is
             when Small_High => False,           -- ERROR: Many not {19;10}
                                                 --     not allowed values.
             when others => True);
      Sink (case X is
             when 1 | 2 | 4 | 8 => True,         -- OK. {19;9}
             when others => False);

      Sink (case Y is
             when Yellow => True,
             when Orange => False,
             when Red => False);                 -- OK.   {3:12;1}
      Sink (case Y is
             when Yellow => False,
             when Orange => False,
             when Red => False,
             when Green => True,                 -- ERROR: Not a warm  {19;9}
             when others => False);              --        color.
      Sink (case Y is
             when Warm_Color => False);          -- OK.  {19;11}
      Sink (case Y is
             when Orange .. Green => False,      -- ERROR: Green not a  {19;10}
                                                 --        warm color.
             when Red => False);
      Sink (case Y is
             when Red .. Blue => False,          -- ERROR: Blue & Green {19;10}
                                                 --        not a primary color.
             when others => False);
      Sink (case Y is
             when Yellow .. Red => False,
             when Rainbow => True);              -- ERROR: Not all  {19;10}
                                                 --        warm colors.
      Sink (case Z is
             when Single => False,               -- ERROR: 1 not   {19;10}
             when Double_Only => False,          --        double.
             when others => True);
      Sink (case Z is
             when 25 | 50 => True,               -- ERROR: 50 not  {19;9}
             when 30 .. 40 => False,             --        double.
             when others => False);
      Sink (case Z is
             when 2 .. 40 => True,               -- OK.  {19;9}
             when others => False);
      Sink (case Z is
             when 2 => True,
             when Treble => False,               -- ERROR: 41 .. 60 not {19;10}
             when others => False);              --        double.
   end No_Extra_Values;


   procedure Non_Static (X : Integer; Y : Colors; Z : Score_Base) is
      -- Check 5.4(5/3) (again via 4.5.7(19/3))
      -- for the case of dynamic subtypes.
   begin
      Sink (case X is
             when Dyn_Low => True,               -- ERROR: Not static.  {19;9}
             when others => False);
      Sink (case Small_Low'(X) is
             when Dyn_Low_Half  => True,         -- ERROR: Not static.  {19;10}
             when 6 .. 10       => False);
      Sink (case Small_Low'(X) is
             when 7 .. Dyn_Ten => True,          -- ERROR: Not static.  {19;9}
             when 0 | 1 .. 6   => False);

      Sink (case Y is
             when White | Black => False,
             when Dyn_Rainbow => True);          -- ERROR: Not static.  {19;10}
      Sink (case Y is
             when White | Black => False,
             when Dyn_Warm    => True,           -- ERROR: Not static.  {19;12}
             when Cool_Color  => False);
      Sink (case Y is
             when White | Black => False,
             when Green .. V    => True,         -- ERROR: Not static.  {19;12}
             when Warm_Color    => False);
      Sink (case Y is
             when White | Black   => False,
             when Green .. Indigo => True,
             when V               => True,       -- ERROR: Not static.  {19;23}
             when Warm_Color      => False);

      Sink (case Z is
             when Dyn_Not_Single => False,       -- ERROR: Not static.  {19;10}
             when others => True);
      Sink (case Z is
             when 0 .. Single_High => True,      -- ERROR: Not static.  {19;9}
             when Treble_Only => False,
             when others => False);
      Sink (case Z is
             when Single => True,
             when 0 | Treble_Only => False,
             when Highest+1 | 62 | 63 => False); -- ERROR: Not static.  {19;11}
   end Non_Static;

begin
   Full_Coverage_1 (2, Red, 50);

   Full_Coverage_2 (12, Green, 33);

   No_Extra_Values (8, Yellow, 14);

   Non_Static (3, Blue, 25);

end B457004;
