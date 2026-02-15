-- BC510220.A
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
--      Check that the following entities that are of a formal incomplete
--      type are illegal: a component, an object created by an object
--      declaration or an allocator, generic formal objects, the result of
--      a function call, the result type of a function body, and for formal
--      untagged incomplete types (only), parameters of subprogram bodies
--      and actual parameters in calls.
--
-- TEST DESCRIPTION:
--      The following are illegal:
--       (A) Components of a formal incomplete type for both
--           record and array types.
--       (B) Object declarations of a formal incomplete type.
--       (C) Allocators of formal incomplete type.
--       (D) Generic formal objects of modes in and in out of an formal
--           incomplete type.
--       (E) Function bodies whose result type is a formal incomplete type.
--       (F) The result of a function call that is of a formal incomplete type.
--       (G) Parameters of subprogram bodies of a formal untagged incomplete
--           type (these are legal for a formal tagged incomplete type).
--       (H) The actual parameter in a call of a formal untagged incomplete
--           type (these are legal for a formal tagged incomplete type).
--      Each of these violates some rule of 3.10.1 for an incomplete view,
--      and a formal incomplete type is an incomplete view within the generic.
--      Each error is marked as to which of these cases is being tested.
--
-- TEST FILES:
--      This test consists of the following files:
--      -> BC510220.A
--         BC510221.A
--         BC510222.A
--         BC510223.AM
--
-- PASS/FAIL CRITERIA:
--      Files BC510220.A, BC510221.A, and BC510222.A contain errors. All
--      errors in these files must be detected to pass the test.
--
--
-- CHANGE HISTORY:
--      15 Jan 2012   GJD     Initial version.
--      25 Apr 2014   RLB     Split into two tests, this one for illegal
--                            uses of formal incomplete types. Made sure
--                            that no unit depends on a unit with errors,
--                            and put units with errors into separate
--                            files.
--
--!

generic
   type Untagged_Incomplete;

   In_Object_1 : in Untagged_Incomplete;                         -- ERROR: (D)

   In_Out_Object_1 : in out Untagged_Incomplete;                 -- ERROR: (D)

   type Formal_Arr_1
     is array (Integer range <>) of Untagged_Incomplete;         -- ERROR: (A)

   type Formal_Acc_1 is access all Untagged_Incomplete;          -- OK.

   type Tagged_Incomplete is tagged;

   In_Object_2 : in Tagged_Incomplete;                           -- ERROR: (D)

   In_Out_Object_2 : in out Tagged_Incomplete;                   -- ERROR: (D)

   type Formal_Arr_2
     is array (Integer range <>) of Tagged_Incomplete;           -- ERROR: (A)

   type Formal_Acc_2 is access all Tagged_Incomplete;            -- OK.

package BC51022_0 is

end BC51022_0;
