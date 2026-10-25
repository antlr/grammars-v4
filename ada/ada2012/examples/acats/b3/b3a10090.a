-- B3A10090.A
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
--     Check that the name of a parameter that has an incomplete view cannot
--     be used as a prefix.
--
--     Check that a dereference of an access-to-incomplete view cannot be used
--     as a prefix.
--
-- TEST DESCRIPTION:
--
--     We test only incomplete views from the limited view of a package
--     in this test. For the first objective, only tagged incomplete views
--     can be used to declare a parameter that can be accessed (in a subprogram
--     body.) We include the second objective here as that is convinient.
--     Other tests try additional cases for the second objective. (See B3A1008
--     for tests involving incomplete types.)
--
-- TEST FILES:
--      This test consists of the following files:
--      -> B3A10090.A
--         B3A10091.A
--
-- PASS/FAIL CRITERIA:
--     Test file B3A10091 contains errors. All errors in this files must be
--     detected.
--
-- CHANGE HISTORY:
--     13 Jan 2015  RLB  Created test.
--
package B3A1009_A is

   type An_Array is array (Positive range <>) of Integer;

   type A_Rec is record
      C : Character;
   end record;

   type A_Rec_w_Disc (D : Boolean) is record
      N : Natural;
   end record;

   type A_Tagged is tagged record
      W : Wide_Character;
   end record;

   type A_Tagged_w_Disc (D : Boolean) is tagged record
      F : Float;
   end record;

   type A_Priv_Tagged_w_Disc (D : Integer) is tagged private;

   type An_Interf is limited interface;

private
   type A_Priv_Tagged_w_Disc (D : Integer) is tagged record
      A : String(1..D);
   end record;
end B3A1009_A;
