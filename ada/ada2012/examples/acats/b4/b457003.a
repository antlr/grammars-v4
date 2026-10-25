-- B457003.A
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
--     Check that the else part of an if expression cannot be omitted if the
--     type of the expression is not a boolean type.
--
-- TEST DESCRIPTION:
--     We try various cases where "else True" would be legal but the type
--     is not a boolean type.
--
-- CHANGE HISTORY:
--      02 Jan 2015   RLB     Created test.
--
--!

procedure B457003 is

   type Der_Bool is new Boolean; -- Is a boolean type.

   type Like_Bool is (False, True, Maybe);

   procedure Cond (B : in Like_Bool) is null;

   function True return Natural is (12);

   procedure Sink (N : in Natural) is null;

   type Boolish is record
      C1, C2 : Boolean;
   end record;

   function True return Boolish is (Boolish'(True, True));

   procedure Eaten (B : in Boolish) is null;

   A, B : Boolean := False;

   D : Der_Bool := True;

   L, M : Like_Bool := Maybe;

   N, P : Natural := 1;

   H, I : Boolish := (False, False);

begin

   if (if A then B else True) then       -- OK.
      null;
   end if;

   if (if A then B) then                 -- OK.
      null;
   end if;

   if (if A then D else True) then       -- OK.
      null;
   end if;

   if (if A then D) then                 -- OK.
      null;
   end if;

   B := (if A then False);               -- OK.

   D := (if A then False);               -- OK.

   M := (if A then L else True);         -- OK.

   M := (if A then L);                   -- ERROR:

   P := (if A then N else True);         -- OK.

   P := (if A then N);                   -- ERROR:

   I := (if A then H else True);         -- OK.

   I := (if A then H);                   -- ERROR:

   Cond (if A then L else True);         -- OK.

   Cond (if A then L);                   -- ERROR:

   Sink (if A then N else True);         -- OK.

   Sink (if A then N);                   -- ERROR:

   Eaten (if A then H else True);        -- OK.

   Eaten (if A then H);                  -- ERROR:

   declare
      True : constant Float := 0.0;
   begin
      if Float'(if A then 3.14159 else True) /= 1.0 then -- OK.
         null;
      elsif Float'(if A then 3.14159) /= 2.0 then -- ERROR:
         null;
      end if;
   end;

end B457003;
