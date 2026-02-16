-- B730010.A
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
-- OBJECTIVE:
--      Check that if the full_type_declaration for a private extension
--      includes a derived_type_definition, then the reserved word limited
--      shall appear in the full_type_declaration if it also appears in the
--      private_extension_declaration.
--
-- TEST DESCRIPTION:
--      We declare a number of private types, some of them private extensions,
--      and some not, and then make sure that the completion of the
--      private extension requires the reserved word limited if and only
--      if the extension has it, and that the completion of the private types
--      have no such requirement.
--
--      This test is checking 7.3(10.1), AI95-00419-01.
--
-- CHANGE HISTORY:
--      19 Feb 15   DRE     Split from test B730009.
--      18 Mar 15   RLB     Added additional cases, issued test.
--
package B730010 is
   type Lim is tagged limited null record;

   type L_Iface is limited interface;
   type Type_Implementing_L_IFace is limited new L_IFace with null record;

   type Priv1 is limited new L_IFace with private;
   type Priv2 is limited private;
   type Priv3 is limited new Lim with private;
   type Priv4 is limited private;

   type Priv5 is new Lim with private;
   type Priv6 is private;

   type Priv7 is new Lim with private;
   type Priv8 is limited new Lim with private;

private

   type Priv1 is new Type_Implementing_L_IFace   -- ERROR:
      with null record;              -- reserved word Limited is required

   type Priv2 is new Type_Implementing_L_IFace   -- OK.
      with null record;              -- not a private extension

   type Priv3 is new Lim                         -- ERROR:
      with null record;              -- reserved word Limited is required

   type Priv4 is new Lim                         -- OK.
      with null record;              -- not a private extension

   type Priv5 is limited new Lim                 -- ERROR:
      with null record;              -- reserved word Limited is not allowed

   type Priv6 is limited new Lim                 -- ERROR:
      with null record;              -- full type cannot be limited

   type Priv7 is new Lim                         -- OK.
      with null record;              -- neither view has reserved word Limited

   type Priv8 is limited new Lim                 -- OK.
      with null record;              -- both views have reserved word Limited

end B730010;
