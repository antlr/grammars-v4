-- B860001.A
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
-- OBJECTIVE
--    Check that the selecting_expression of a case expression cannot be
--    resolved if information from the choices is required to resolve it.
--
--    Check that the selecting_expression of a case statement cannot be
--    resolved if information from the choices is required to resolve it.
--
-- TEST DESCRIPTION
--    This test checks 8.6(9/4), as modified by AI12-0040-1 (part of the
--    2015 Corrigendum). (The original rule dates back to Ada 83.)
--
--    The third pair of examples is exactly as given in the AI, the other
--    two pairs are inspired by it. Note that in all of these cases, the
--    case choices provide enough information to determine which of the
--    overloaded functions is intended -- Ada does not allow using that
--    information.
--
-- HISTORY:
--      02 Dec 2015   RLB   Created test from AI example.

procedure B860001 is

   type E1 is (Aa, Bb, Cc);
   type E2 is (Bb, Cc, Dd);

   function F return E1 is (E1'First);
   function F return E2 is (E2'First);

   function G return E1 is (E1'Last);
   function G return Integer is (12);

   N : Natural;

begin
   N := (case G is                                               -- ERROR:
           when 1 => 222,
           when 2 => 444,
           when others => 666);

   case G is                                                     -- ERROR:
      when 1 => null;
      when 2 => null;
      when others => null;
   end case;

   N := (case G is                                               -- ERROR:
           when Aa => 987,
           when Bb => 654,
           when Cc => 321);

   case G is                                                     -- ERROR:
      when Aa => null;
      when Bb => null;
      when Cc => null;
   end case;

   N := (case F is                                               -- ERROR:
           when Aa => 123,
           when Bb => 456,
           when Cc => 789);

   case F is                                                     -- ERROR:
      when Aa => null;
      when Bb => null;
      when Cc => null;
   end case;

   N := (case E1'(F) is                                          -- OK.
           when Aa => 123,
           when Bb => 456,
           when Cc => 789);

   case E1'(F) is                                                -- OK.
      when Aa => null;
      when Bb => null;
      when Cc => null;
   end case;

end B860001;
