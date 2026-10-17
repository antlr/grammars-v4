-- BXB0001.A
--
--                             Grant of Unlimited Rights
--
--     Ada Core Technologies Inc. (AdaCore) holds unlimited rights in the
--     software and documentation contained herein. Unlimited rights are
--     the same as those granted by the U.S. Government for older parts of
--     the Ada Conformity Assessment Test Suite, and are defined in DFAR
--     252.227-7013(a)(19). By making this public release, AdaCore intends
--     to confer upon all recipients unlimited rights equal to those held by
--     the Ada Conformity Assessment Authority. These rights include rights
--     to use, duplicate, release or disclose the released technical data and
--     computer software in whole or in part, in any manner and for any purpose
--     whatsoever, and to have or permit others to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS. ADACORE MAKES NO EXPRESS OR IMPLIED WARRANTY AS
--     TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE SOFTWARE,
--     DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE OR
--     DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
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
--     This test is based on one submitted by AdaCore; AdaCore retains
--     the copyright on the test.
--
--*
--  OBJECTIVE:
--     Check that an Unchecked_Union is illegal if there is any component
--     that is not an unchecked union and whose constraint depends on a
--     discriminant.
--
--  TEST DESCRIPTION:
--     This checks B.3.3(8/2). This rule uses "per-object constraint"
--     (POC in the comments below) rather than "constraint that depends on
--     a discriminant"; these are effectively the same thing. The only other
--     sort of per-object constraint is an access-to-current-instance, and
--     as an unchecked union cannot complete an incomplete or private type
--     by B.3.3(11/2), there is no way for a type to have an access
--     discriminant of an unchecked union type. Thus no such per-object
--     constraints are possible.
--
--     B.3.3(7/2) requires that all components have nominal subtypes that are
--     C-compatible. To ensure this, we use types from Interfaces.C and types
--     with explicit convention C. Other types could be C-compatible if the
--     "implementation permits it" (B.1(20)), but that is not suitable for
--     an ACATS test.
--
--  APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support package
--     Interfaces.C, aspect Convention C on record types, and aspect
--     Unchecked_Union.
--
--  CHANGE HISTORY:
--     29 JUL 2004  H K  Initial Version.
--     22 APR 2015  RLB  Modernized objective, ensured that all components
--                       of unions are C-compatible, added applicability
--                       criteria, corrected error labels.
--!
with Interfaces.C; use Interfaces.C;                    -- N/A => ERROR.
package BXB3001 is

   --  A simple discriminated record type

   type Rec_Type (Discr : Int := 0) is record
      Comp_Rec_Type_1 : Int := 1;
   end record
      with Convention => C;                             -- N/A => ERROR.

   --  An Unchecked_Union type with simple components

   type UU_Type (Discr : Int := 0) is record
      Comp_UU_Type_1 : Int := 1;
      case Discr is
         when 0 =>
            Comp_UU_Type_2 : Int := 2;
         when others =>
            Comp_UU_Type_3 : Int := 3;
      end case;
   end record
      with Unchecked_Union, Convention => C;

   --  An enclosing Unchecked_Union type used for the test

   type Encl_UU_Type (Discr : Int := 0) is record
      Comp_Encl_UU_Type_1 : Int := 1;
      Comp_Encl_UU_Type_2 : Rec_Type;                  -- OK.
                 -- Unconstr, non-U_U
      Comp_Encl_UU_Type_3 : Rec_Type (3);              -- OK.
                 -- Constr, non-U_U, non-POC
      Comp_Encl_UU_Type_4 : Rec_Type (Discr);          -- ERROR:
                 -- Constr, non-U_U, POC
      Comp_Encl_UU_Type_5 : UU_Type;                   -- OK.
                 -- Unconstr, U_U
      Comp_Encl_UU_Type_6 : UU_Type (6);               -- OK.
                 -- Constr, U_U, non-POC
      Comp_Encl_UU_Type_7 : UU_Type (Discr);           -- OK.
                 -- Constr, U_U, POC
      case Discr is
         when 0 =>
            Comp_Encl_UU_Type_8  : Int := 8;
         when 1 =>
            Comp_Encl_UU_Type_9  : Rec_Type;           -- OK.
                  -- Unconstr, non-U_U
         when 2 =>
            Comp_Encl_UU_Type_10 : Rec_Type (10);      -- OK.
                  -- Constr, non-U_U, non-POC
         when 3 =>
            Comp_Encl_UU_Type_11 : Rec_Type (Discr);   -- ERROR:
                  -- Constr, non-U_U, POC
         when 4 =>
            Comp_Encl_UU_Type_12 : UU_Type;            -- OK.
                  -- Unconstr, U_U
         when 5 =>
            Comp_Encl_UU_Type_13 : UU_Type (14);       -- OK.
                  -- Constr, U_U, non-POC
         when others =>
            Comp_Encl_UU_Type_14 : UU_Type (Discr);    -- OK.
                  -- Constr, U_U, POC
      end case;
   end record
      with Unchecked_Union;

end BXB3001;
