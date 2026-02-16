-- BXB3004.A
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
--  OBJECTIVE:
--     Check that the aspect Unchecked_Union cannot be specified illegally:
--       (A) It cannot be specified on a non-record type;
--       (B) It cannot be specified on a record type that does not
--           have discriminants;
--       (C) It cannot be specified on a record type that have discriminants
--           but does not have a variant part;
--       (D) It cannot be specified for a derived record type whose parent
--           type is a discriminated record type with a variant part that
--           has primitive operations;
--           Note: This rule will be repealed in Ada 202x by AI12-0376-1, so
--           we no longer test it.
--       (E) Its expression cannot have a type other than Boolean;
--       (F) Its expression cannot be a non-static Boolean expression.
--
--  TEST DESCRIPTION:
--     This checks B.3.3(3.1-3.2/3). [(D) tests 13.1(10), which is triggered
--     by the "representation aspect" wording.] We try various cases to try
--     all of the errors above.
--
--     B.3.3(7/2) requires that all components of an unchecked union have
--     nominal subtypes that are C-compatible. To ensure this, we use types
--     from Interfaces.C; we also could have usedtypes with explicit
--     convention C. Other types could be C-compatible if the "implementation
--     permits it" (B.1(20)), but that is not suitable for an ACATS test.
--
--  APPLICABILITY CRITERIA:
--     This test is applicable to implementations that support package
--     Interfaces.C and aspect Unchecked_Union.
--
--  CHANGE HISTORY:
--     22 APR 2015  RLB  Created test.
--     14 May 2020  RLB  Removed (D) test case as the rule will be repealed
--                       by AI12-0376-1. Added error location indicators.
--!
with Interfaces.C; use Interfaces.C;             -- N/A => ERROR. {1}
package BXB3004 is

   ARod : constant Integer := 12;

   type My_Int is range 1 .. 10
      with Unchecked_Union;                               -- ERROR: (A) {1:4;1}

   type Int_Arr is array (1..10) of Int
      with Unchecked_Union;                               -- ERROR: (A) {1:4;1}

   type Priv (D : Int := 0) is private
      with Unchecked_Union;                               -- ERROR: (A) {1:4;1}

   type No_Disc is record
      Comp_1 : Int;
      Comp_2 : Unsigned_Char;
   end record with Unchecked_Union;                       -- ERROR: (B) {3:4;1}

   type No_Var (D : Unsigned_Char := 0) is record
      Comp_1 : Int;
      Comp_2 : Signed_Char;
   end record with Unchecked_Union;                       -- ERROR: (C) {3:4;1}

   type Both (D : Unsigned_Char := 0) is record
      case D is
         when 0 =>
            Alt_1 : Signed_Char;
         when others =>
            Alt_2 : Unsigned_Char;
      end case;
   end record with Unchecked_Union => True;               -- OK. {7:4;1}

   type Not_UU (D : Unsigned_Char := 0) is record
      case D is
         when 0 =>
            Alt_1 : Signed_Char;
         when others =>
            Alt_2 : Unsigned_Char;
      end case;
   end record;

   function Get_NUU (A : Not_UU) return Int is
     (if A.D = 0 then Int(A.Alt_1) else Int(A.Alt_2));
      -- Primitive of Not_UU.

   NVar : Not_UU; -- Freeze Not_UU.

-- type Bad_Der is new Not_UU with Unchecked_Union;       -- ERROR: (D) {4;1}
   --   This will be legal in Ada 202x, so we don't test it nor
   --   require it to work for Ada 2012.

   type OK_Der is new Both with Unchecked_Union => True;  -- OK. {4;1}

   type QB (D : Unsigned_Char := 0) is record
      case D is
         when 0 =>
            Alt_1 : Signed_Char;
         when others =>
            Alt_2 : Unsigned_Char;
      end case;
   end record with Unchecked_Union => ARod;               -- ERROR: (E) {7:4;1}

   type LB (D : Unsigned_Char := 0) is record
      case D is
         when 0 =>
            Alt_1 : Signed_Char;
         when others =>
            Alt_2 : Unsigned_Char;
      end case;
   end record with Unchecked_Union => NVar.D /= 0;        -- ERROR: (F) {7:4;1}

   type Great (D : Unsigned_Char := 0) is record
      case D is
         when 0 =>
            Alt_1 : Signed_Char;
         when others =>
            Alt_2 : Unsigned_Char;
      end case;
   end record with Unchecked_Union => ARod = 12;          -- OK. {7:4;1}

private

   type Priv (D : Int := 0) is record                     
      case D is
         when 0 =>
            Alt_1 : Signed_Char;
         when others =>
            Alt_2 : Unsigned_Char;
      end case;
   end record;                                            -- OK. {7:4;1}

end BXB3004;

