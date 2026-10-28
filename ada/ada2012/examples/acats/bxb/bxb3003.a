-- BXB3003.A
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
--     Check that the completion of an incomplete type with a known
--     discriminant part cannot be an unchecked union type.
--
--     Check that the full type for a private type with a known discriminant
--     part cannot be an unchecked union type.
--
--  TEST DESCRIPTION:
--     This checks B.3.3(11/2). We try various kinds of types with all of
--     the possible kinds of discriminant parts (known, unknown, none)
--     completed with unchecked union types.
--
--     B.3.3(7/2) requires that all components of an unchecked union have
--     nominal subtypes that are C-compatible. To ensure this, we use types
--     from Interfaces.C; and we could have used types with explicit
--     convention C. Other types could be C-compatible if the "implementation
--     permits it" (B.1(20)), but that is not suitable for an ACATS test.
--
--  APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support package
--     Interfaces.C and aspect Unchecked_Union.
--
--  CHANGE HISTORY:
--     29 JUL 2004  H K  Initial Version.
--     22 APR 2015  RLB  Modernized objective, ensured that all components
--                       of unions are C-compatible, added applicability
--                       criteria, corrected error labels.
--!
with Interfaces.C; use Interfaces.C;             -- N/A => ERROR.
package BXB3003 is

   type Incmpl_1;                                -- Incomplete, undiscr
   type Incmpl_2 (Discr : Int := 0);             -- Incomplete, knwn discr
   type Incmpl_3 (<>);                           -- Incomplete, unknwn discr

   type Priv_1 is private;                       -- Private, undiscr
   type Priv_2 (Discr : Int := 0) is private;    -- Private, knwn discr
   type Priv_3 (<>) is private;                  -- Private, unknwn discr

   type Incmpl_1 (Discr : Int := 0) is record    -- OK.
      Comp_1 : Int := 1;
      case Discr is
         when 0 =>
            Comp_2 : Int := 2;
         when others =>
            Comp_3 : Int := 3;
      end case;
   end record with Unchecked_Union;

   type Incmpl_2 (Discr : Int := 0) is record    -- ERROR:
      Comp_1 : Int := 1;
      case Discr is
         when 0 =>
            Comp_2 : Int := 2;
         when others =>
            Comp_3 : Int := 3;
      end case;
   end record with Unchecked_Union;

   type Incmpl_3 (Discr : Int := 0) is record    -- OK.
      Comp_1 : Int := 1;
      case Discr is
         when 0 =>
            Comp_2 : Int := 2;
         when others =>
            Comp_3 : Int := 3;
      end case;
   end record with Unchecked_Union;

private

   type Priv_1 (Discr : Int := 0) is record      -- OK.
      Comp_1 : Int := 1;
      case Discr is
         when 0 =>
            Comp_2 : Int := 2;
         when others =>
            Comp_3 : Int := 3;
      end case;
   end record with Unchecked_Union;

   type Priv_2 (Discr : Int := 0) is record      -- ERROR:
      Comp_1 : Int := 1;
      case Discr is
         when 0 =>
            Comp_2 : Int := 2;
         when others =>
            Comp_3 : Int := 3;
      end case;
   end record with Unchecked_Union;

   type Priv_3 (Discr : Int := 0) is record      -- OK.
      Comp_1 : Int := 1;
      case Discr is
         when 0 =>
            Comp_2 : Int := 2;
         when others =>
            Comp_3 : Int := 3;
      end case;
   end record with Unchecked_Union;

end BXB3003;
