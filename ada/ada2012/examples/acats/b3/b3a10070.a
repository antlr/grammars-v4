-- B3A10070.A
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
--
--     Check that constraints or exclusions other than discriminant constraints
--     cannot be used on the name of an incomplete view when used as the
--     subtype mark in an access-to-object definition.
--
--     Check that constraints (other than appropriate discriminant constraints)
--     cannot be used on an access-to-incomplete type.
--
--     When the name of an incomplete view is used to declare a subtype, check
--     that any constraint or null exclusion is illegal.
--
-- TEST DESCRIPTION:
--
--     We test both normal and tagged incomplete types, and untagged and
--     tagged incomplete views from the limited view of a package. In each
--     case, we try a constraint or exclusion that would have been appropriate
--     for the full type. Note that we cannot test this on anonymous
--     access types; the syntax is "subtype_mark" rather than
--     "subtype_indication".
--
--     The original test tried delta and digits constraints; we didn't include
--     these as they've been obsolete forever and it's unlikely a compiler
--     would reject ranges and allow them.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> B3A10070.A
--         B3A10071.A
--
-- PASS/FAIL CRITERIA:
--     Test file B3A10071 contains errors. All errors in this files must be
--     detected.
--
-- CHANGE HISTORY:
--     13 Mar 2014  RLB  Created test for the remaining objectives of B38105A.
--
package B3A1007_A is

   type An_Int is range -1000 .. 1000;

   type A_Float is new Float;

   type A_Fix is delta 1.0 range -2.0 .. 2.0;

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

   type An_Access_Rec is access A_Rec;

end B3A1007_A;
