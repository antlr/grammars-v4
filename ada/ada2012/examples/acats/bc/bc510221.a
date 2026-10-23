-- BC510221.A
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
-- OBJECTIVE
--      See BC510220.A.
--
-- TEST DESCRIPTION
--      See BC510220.A.
--
-- TEST FILES:
--      This test consists of the following files:
--         BC510220.A
--      -> BC510221.A
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

   with function Constructor_1 return Untagged_Incomplete;

   type Tagged_Incomplete is tagged;

   with function Constructor_2 return Tagged_Incomplete;

package BC51022_1 is

   package Uses_Of_Untagged_Incomplete_Formal_Type is

      Var_Object : Untagged_Incomplete;                           -- ERROR: (B)

      Const_Object : constant Untagged_Incomplete
         := Constructor_1;                                        -- ERROR: (B) & (F)

      type Rec is record
         Component : Untagged_Incomplete;                         -- ERROR: (A)
      end record;

      type Arr is array (Integer range <>) of Untagged_Incomplete;-- ERROR: (A)

      type Acc_Incomplete is access all Untagged_Incomplete;      -- OK.

      Acc : Acc_Incomplete := new Untagged_Incomplete;            -- ERROR: (C)

   end Uses_Of_Untagged_Incomplete_Formal_Type;

   package Uses_Of_Tagged_Incomplete_Formal_Type is

      Var_Object : Tagged_Incomplete;                             -- ERROR: (B)

      Const_Object : constant Tagged_Incomplete
         := Constructor_2;                                        -- ERROR: (B) & (F)

      type Rec is record
         Component : Tagged_Incomplete;                           -- ERROR: (A)
      end record;

      type Arr is array (Integer range <>) of Tagged_Incomplete;  -- ERROR: (A)

      type Acc_Incomplete is access all Tagged_Incomplete;        -- OK.

      Acc : Acc_Incomplete := new Tagged_Incomplete;              -- ERROR: (C)

   end Uses_Of_Tagged_Incomplete_Formal_Type;

end BC51022_1;


