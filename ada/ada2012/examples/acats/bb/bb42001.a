-- BB42001.A
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
--     Check that the arguments of a pragma Assert have to have the correct
--     types.
--
-- CHANGE HISTORY:
--     25 Jan 2016   RLB   Created test.
--     28 Mar 2016   RLB   Added error location codes.
--     20 Jan 2017   RLB   Corrected two incorrect error location codes.
--!
procedure BB42001 is

   type My_Bool is new Boolean;
   My_Obj : My_Bool := True;

   function Do_It return Integer is
   begin
      return 1;
   end Do_It;

   function Mess return Float is
   begin
      return 3.14159;
   end Mess;

   procedure Loop_Counter (Bound : in Integer) is
      -- Try a pragma Assert in place of a statement.
      Iters : Integer := 0;
   begin
      -- We simulate a algorithm where the author has forgotten about
      -- the possibility of negative input values. We try various typos.
      for Index in 1 .. Bound loop
         Iters := Iters + 1;
      end loop;
      pragma Assert (Iters = Bound, Message =>
         "Wrong iters=" & Integer'Image(Iters));              -- OK. {1:22;2}

      pragma Assert (Iters);                                  -- ERROR: {22;2}

      pragma Assert (Iters - Bound);                          -- ERROR: {22;2}

      pragma Assert (My_Obj);                                 -- OK. {22;2}

      pragma Assert (Do_It);                                  -- ERROR: {22;2}

      pragma Assert (Iters = Bound, Message => Mess);         -- ERROR: {48;2}

      pragma Assert (Iters = Bound, Message => Iters);        -- ERROR: {48;2}

      pragma Assert (My_Obj and True,
                     Message => Integer'Image(Iters));        -- OK. {1:22;2}

   end Loop_Counter;


   function Get_Die_Roll return Positive is
      Roll : Float := 0.05;
      Result : Integer := Integer(Roll*6.0);

      -- Simulating an off-by-one error; pragma Assert in place of
      -- a declarative item.
      pragma Assert (Check => Result in 1 .. 6);              -- OK. {22;2}

      pragma Assert (Result);                                 -- ERROR: {22;2}

      pragma Assert (Roll, Message => "Oh boy");              -- ERROR: {22;23}

      pragma Assert (Roll > Mess, Message => Float'Image(Roll));-- OK. {22;2}

      pragma Assert (Result <= 6, Message => Mess);           -- ERROR: {46;2}

   begin
      return Result;
   end Get_Die_Roll;


begin
   Loop_Counter (Get_Die_Roll);
end BB42001;

