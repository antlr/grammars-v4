-- BXB3002.A
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
--     Check that the name of a discriminant of an unchecked union cannot be
--     used outside of the type declaration.
--
--     Check that the name of a discriminant of an unchecked union cannot be
--     used in a record representation clause for the type.
--
--  TEST DESCRIPTION:
--     This checks B.3.3(9/3). The only uses that we try are those
--     that otherwise would be legal. Appropriate unions are declared, and
--     then uses of the discriminant via selected_components and in a record
--     representation clause are tried.
--
--     B.3.3(7/2) requires that all components of an unchecked union have
--     ominal subtypes that are C-compatible. To ensure this, we use types
--     from Interfaces.C; we also could have used types with explicit
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
--                       criteria, corrected error labels, added test case
--                       for record representation clauses.
--!
with Interfaces.C; use Interfaces.C;                    -- N/A => ERROR.
package BXB3002 is

   --  An Unchecked_Union type with simple components.

   type UU_Type (Discr : Int := 0) is record
      Comp_UU_Type_1 : Int := 1;
      case Discr is                                     -- OK.
         when 0 =>
            Comp_UU_Type_2 : Short := 2;
         when others =>
            Comp_UU_Type_3 : Unsigned_Short := 3;
      end case;
   end record
      with Unchecked_Union;

   Obj_1 : UU_Type;
   Obj_2 : UU_Type (1);

   I1 : Int := Obj_1.Discr;                             -- ERROR:
   I2 : Int := Obj_2.Discr;                             -- ERROR:
   I3 : Unsigned_Short := Obj_2.Comp_UU_Type_3;         -- OK.
   I4 : Short := Obj_1.Comp_UU_Type_2;                  -- OK.

   type UU_Rep_Type (D : Int := 0) is record
      case D is                                         -- OK.
         when 0 =>
            Comp_1 : Signed_Char := 2;
         when others =>
            Comp_2 : Unsigned_Char := 3;
      end case;
   end record
      with Unchecked_Union;

   for UU_Rep_Type use record
      Comp_1 at 0 range 0 .. CHAR_BIT-1;                -- OK.
      Comp_2 at 0 range 0 .. CHAR_BIT-1;                -- OK.
      D at 4 range 0 .. Int'Size-1;                     -- ERROR:
   end record;

end BXB3002;
