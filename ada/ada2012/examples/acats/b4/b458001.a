-- B458001.A
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
--     Check that a quantified expression has to be surrounded in parentheses
--     if it is not already surrounded by them.
--
-- TEST DESCRIPTION:
--     We try various contexts where one but not both parentheses are provided
--     by context, along with a few cases where the extra set of parentheses
--     can be omitted. In particular, we try in subprogram parameter lists,
--     array indexing, aggregates, generic parameter lists, and pragma
--     argument lists. (Other contexts like qualified expressions, type
--     conversions, expression functions, and attribute parameters [like the
--     number following First] can only have a single argument and thus always
--     allow dropping the parentheses.)
--
--     Because this is a syntax rule (given with an English-language rule
--     rather than a grammar rule), we only need to test a few examples;
--     exhaustively testing various kinds of quantified expressions would be
--     pointless.
--
--     Also, because this is a syntax rule, we try to put a legal expression
--     between every illegal one, to give error correctors a bit more chance
--     to recover.
--
-- CHANGE HISTORY:
--      21 Dec 2017   RLB     Created test, using B457006 as a basis.
--
--!

procedure B458001 is

   function Rogers return Natural is (12);

   Test : array (1 .. 10) of Integer := (others => Rogers);

   procedure Single (B : in Boolean) is null;

   procedure Double (B : in Boolean; N : in Natural := 0) is null;

   procedure Sink (N : in Natural) is null;

   procedure Double_Sink (N : in Natural; B : in Boolean := True) is null;

   type One_Dim is array (Boolean) of Natural;

   type Two_Dim is array (Boolean, Boolean) of Natural;

   type One_Bool_Dim is array (1..3) of Boolean;

   procedure Utility_Sink (A : in One_Bool_Dim) is null;

   A, B : Boolean := False;

   N, P : Natural := 1;

   A1 : One_Dim := (1, 2);

   A2 : Two_Dim := (others => (others => 0));

   generic
      G1 : in Boolean;
   procedure Gen1;

   procedure Gen1 is null;

   generic
      G1 : in Boolean;
      G2 : in Natural;
   procedure Gen2;

   procedure Gen2 is null;

   generic
      G1 : in Natural;
      G2 : in Boolean;
   procedure Gen3;

   procedure Gen3 is null;

begin

   A := (for all I in Test'range => Test(I) < 20);             -- OK. {9}

   Single ((for all I in Test'range => Test(I) < 20));         -- OK. {4}

   Single (for all I in Test'range => Test(I) < 20);           -- OK. {4}

   Single (B => (for all I in Test'range => Test(I) < 20));    -- OK. {17;1}

   Single (B => for all I in Test'range => Test(I) < 20);      -- ERROR: {17}

   Double ((for all I in Test'range => Test(I) < 20), Rogers); -- OK. {12;9}

   Double (for all I in Test'range => Test(I) < 20, Rogers);   -- ERROR: {11}

   Double_Sink (Rogers,
       (for some I in Test'range => Test(I) > 20));            -- OK. {8}

   Double_Sink (Rogers,
       for some I in Test'range => Test(I) > 20);              -- ERROR: {8}

   Double ((for some I in Test'range => Test(I) > 20));        -- OK. {12;1}

   Double (for some I in Test'range => Test(I) > 20);          -- OK. {11}

   Double (B => (for some I in Test'range => Test(I) > 20));   -- OK. {17;1}

   Double (B => for some I in Test'range => Test(I) > 20);     -- ERROR: {17}


   Single (
      (if B then (for some I in Test'range => Test(I) > 20))); -- OK. {18;1}

   Single (
      (if B then for some I in Test'range => Test(I) > 20));   -- ERROR: {18}

   Single (
      if B then (for some I in Test'range => Test(I) > 20));   -- OK. {7}

   Single (
      if B then for some I in Test'range => Test(I) > 20);     -- ERROR: {7}


   Sink (A1((for all I in Test'range => Test(I) < 20)));       -- OK. {13;2}

   Sink (A1(for all I in Test'range => Test(I) < 20));         -- OK. {13;1}

   Sink (A2(True, (for all I in Test'range => Test(I) < 20))); -- OK. {19;2}

   Sink (A2(True, for all I in Test'range => Test(I) < 20));   -- ERROR: {19}

   Sink (A2((for all I in Test'range => Test(I) < 20), False));-- OK. {13;2}

   Sink (A2(for all I in Test'range => Test(I) < 20, False));  -- ERROR: {13}

   Utility_Sink (
      (True, (for all I in Test'range => Test(I) < 20), True));-- OK. {1:4;8}

   Utility_Sink (
      (True, for all I in Test'range => Test(I) < 20, True)); -- ERROR: {1:4}

   Utility_Sink (
      ((for all I in Test'range => Test(I) < 20), False, True));-- OK. {1:4;15}

   Utility_Sink (
      (for all I in Test'range => Test(I) < 20, False, True));-- ERROR: {1:4}

   Utility_Sink (
      (True, False, (for all I in Test'range => Test(I) < 20)));-- OK. {1:4;2}

   Utility_Sink (
      (True, False, for all I in Test'range => Test(I) < 20)); -- ERROR: {1:4}

   declare
      procedure I1 is new
         Gen1 ((for some I in Test'range => Test(I) > 20));    -- OK. {15}
      procedure I2 is new
         Gen1 (for some I in Test'range => Test(I) > 20);      -- OK. {15}
      procedure I3 is new
         Gen1 (G1 => (for some I in Test'range => Test(I) > 20)); -- OK. {26;1}
      procedure I4 is new
         Gen1 (G1 => for some I in Test'range => Test(I) > 20); -- ERROR: {26}
   begin
      null;
   end;

   declare
      procedure I5 is new
         Gen3 (52, (for some I in Test'range => Test(I) > 20));-- OK. {20;1}
      procedure I6 is new
         Gen3 (52, for some I in Test'range => Test(I) > 20);  -- ERROR: {20}
      procedure I7 is new
         Gen2 ((for some I in Test'range => Test(I) > 20), 4); -- OK. {15;4}
      procedure I8 is new
         Gen2 (for some I in Test'range => Test(I) > 20, 4);   -- ERROR: {15}
   begin
      null;
   end;

   pragma Assert ((for all I in Test'range => Test(I) < 20));  -- OK. {18;1}
   pragma Assert (for all I in Test'range => Test(I) < 20);    -- OK. {18}
   pragma Assert
      (Check => (for all I in Test'range => Test(I) < 20));    -- OK. {17;1}
   pragma Assert
      (Check => for all I in Test'range => Test(I) < 20);      -- ERROR: {17}

   pragma Assert ((for all I in Test'range => Test(I) < 20),   -- OK. {18;1}
                 "A message");
   pragma Assert (for all I in Test'range => Test(I) < 20,  -- ERROR: {18;-1:0}
                 "A message");

end B458001;
