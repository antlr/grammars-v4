-- BB42002.A
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
--
-- OBJECTIVE:
--     Check that the arguments of a pragma Assert cannot be reordered even
--     named notation is used.
--
-- CHANGE HISTORY:
--     25 Jan 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--
--!
procedure BB42002 is

   procedure Loop_Counter (Bound : in Integer) is
      -- Try a pragma Assert in place of a statement.
      Iters : Integer := 0;
   begin
      -- We simulate a algorithm where the author has forgotten about
      -- the possibility of negative input values.
      for Index in 1 .. Bound loop
         Iters := Iters + 1;
      end loop;
      pragma Assert (Iters = Bound,
         "Wrong iters=" & Integer'Image(Iters));                -- OK. {1:7}

      pragma Assert (Iters = Bound, Message =>
         "Wrong iters=" & Integer'Image(Iters));                -- OK. {1:7}

      pragma Assert (Check => Iters = Bound, Message =>
         "Wrong iters=" & Integer'Image(Iters));                -- OK. {1:7}

      pragma Assert (Check => Iters = Bound,                    -- 2.8(4/3)
         "Wrong iters=" & Integer'Image(Iters));                -- ERROR: {1:7}

      pragma Assert ("Wrong iters=" & Integer'Image(Iters),
         Iters = Bound);                                        -- ERROR: {1:7}

      pragma Assert (Message => "Wrong iters=" &
         Integer'Image(Iters), Check => Iters = Bound);         -- ERROR: {1:7}

      pragma Assert (Message => "Wrong iters=" &                -- 2.8(4/3)
         Integer'Image(Iters), Iters = Bound);                  -- ERROR: {1:7}

      pragma Assert ("Wrong iters=" & Integer'Image(Iters),
         Check => Iters = Bound);                               -- ERROR: {1:7}

   end Loop_Counter;

begin
   Loop_Counter (12);
end BB42002;

