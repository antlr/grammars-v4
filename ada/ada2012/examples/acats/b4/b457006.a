-- B457006.A
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
--     Check that an if expression has to be surrounded in parentheses if it is
--     not already surrounded by them.
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
--     rather than a grammar rule), we only need to test a few types;
--     exhaustively testing various kinds of types would be pointless.
--
--     Also, because this is a syntax rule, we try to put a legal expression
--     between every illegal one, to give error correctors a bit more chance
--     to recover.
--
-- CHANGE HISTORY:
--      30 Nov 2015   RLB     Created test.
--
--!

procedure B457006 is

   type Color is (Red, Blue, Green);

   procedure Single (C : in Color) is null;

   procedure Double (C : in Color; N : in Natural := 0) is null;

   function Rogers return Natural is (12);

   procedure Sink (N : in Natural) is null;

   procedure Double_Sink (N : in Natural; C : in Color := Red) is null;

   type One_Dim is array (Color) of Natural;

   type Two_Dim is array (Color, Color) of Natural;

   procedure Utility_Sink (A : in One_Dim) is null;

   A, B : Boolean := False;

   L, M : Color := Blue;

   N, P : Natural := 1;

   A1 : One_Dim := (1, 2, 3);

   A2 : Two_Dim := (others => (others => 0));

   generic
      G1 : in Color;
   procedure Gen1;

   procedure Gen1 is null;

   generic
      G1 : in Color;
      G2 : in Natural;
   procedure Gen2;

   procedure Gen2 is null;


begin

   L := (if A then M else Green);                                    -- OK.

   Single ((if A then M else Green));                                -- OK.

   Single (if A then M else Green);                                  -- OK.

   Single (C => (if A then M else Green));                           -- OK.

   Single (C => if A then M else Green);                             -- ERROR:

   Double (Green, (if A then 4 else Rogers));                        -- OK.

   Double (Green, if A then 4 else Rogers);                          -- ERROR:

   Double ((if A then M else Green), Rogers);                        -- OK.

   Double (if A then M else Green, Rogers);                          -- ERROR:

   Double_Sink ((if B then 4 else Rogers));                          -- OK.

   Double_Sink (if B then 4 else Rogers);                            -- OK.

   Double_Sink (if B then 4 else Rogers);                            -- OK.

   Double_Sink (N => if B then 4 else Rogers);                       -- ERROR:

   Sink (A1((if A then L else Red)));                                -- OK.

   Sink (A1(if A then L else Red));                                  -- OK.

   Sink (A2(Blue, (if A then L else Red)));                          -- OK.

   Sink (A2(Blue, if A then L else Red));                            -- ERROR:

   Sink (A2((if A then L else Red), Green));                         -- OK.

   Sink (A2(if A then L else Red, Green));                           -- ERROR:

   Utility_Sink ((15, (if B then 4 else Rogers), 92));               -- OK.

   Utility_Sink ((15, if B then 4 else Rogers, 92));                 -- ERROR:

   Utility_Sink (((if B then 4 else Rogers), 3, 92));                -- OK.

   Utility_Sink ((if B then 4 else Rogers, 3, 92));                  -- ERROR:

   Utility_Sink ((66, 3, (if B then 4 else Rogers)));                -- OK.

   Utility_Sink ((66, 3, if B then 4 else Rogers));                  -- ERROR:

   declare
      procedure I1 is new Gen1 ((if A then L else Red));             -- OK.
      procedure I2 is new Gen1 (if A then L else Red);               -- OK.
      procedure I3 is new Gen1 (G1 => (if A then L else Red));       -- OK.
      procedure I4 is new Gen1 (G1 => if A then L else Red);         -- ERROR:
   begin
      null;
   end;

   declare
      procedure I5 is new Gen2 (Green, (if B then 4 else Rogers));   -- OK.
      procedure I6 is new Gen2 (Green, if B then 4 else Rogers);     -- ERROR:
      procedure I7 is new Gen2 ((if B then L else Red), 4);          -- OK.
      procedure I8 is new Gen2 (if B then L else Red, 4);            -- ERROR:
   begin
      null;
   end;

   pragma Assert ((if A then B));                                    -- OK.
   pragma Assert (if A then B);                                      -- OK.
   pragma Assert (Check => (if A then B));                           -- OK.
   pragma Assert (Check => if A then B);                             -- ERROR:

   pragma Assert ((if A then B), "A message");                       -- OK.
   pragma Assert (if A then B, "A message");                         -- ERROR:

   pragma Assert (A, (if B then "Foo" else "Bar"));                  -- OK.
   pragma Assert (A, if B then "Foo" else "Bar");                    -- ERROR:

end B457006;
