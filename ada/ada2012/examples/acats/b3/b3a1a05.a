-- B3A1A05.A
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
--     Check that the name of an incomplete view cannot be used in the
--     subtype_indication of a generic formal object.
--
-- TEST DESCRIPTION:
--
--     We test both normal and tagged incomplete types, and untagged and
--     tagged incomplete views from the limited view of a package.
--
-- CHANGE HISTORY:
--     13 Mar 2014  RLB  Created test so that B38105A can be removed rather
--                       than repaired.
--
limited with F3A1A00;
package B3A1A05 is

   type Untagged_Inc;

   type Tagged_Inc is tagged;

   generic
      O01 : in Untagged_Inc;                             -- ERROR:
      O02 : in Tagged_Inc;                               -- ERROR:
      O03 : in access Untagged_Inc;                      -- OK.
      O04 : in access Tagged_Inc;                        -- OK.
      O05 : in out Untagged_Inc;                         -- ERROR:
      O06 : in out Tagged_Inc;                           -- ERROR:
      O07 : in out access Untagged_Inc;                  -- OK.
      O08 : in out access Tagged_Inc;                    -- OK.
      O11 : in F3A1A00.An_Untagged_Type;                 -- ERROR:
      O12 : in F3A1A00.A_Tagged_Type;                    -- ERROR:
      O13 : in access F3A1A00.An_Untagged_Type;          -- OK.
      O14 : in access F3A1A00.A_Tagged_Type;             -- OK.
      O15 : in out F3A1A00.An_Untagged_Type;             -- ERROR:
      O16 : in out F3A1A00.A_Tagged_Type;                -- ERROR:
      O17 : in F3A1A00.An_Access_to_Untagged;            -- ERROR:
      O18 : in F3A1A00.An_Access_to_Tagged;              -- ERROR:
      O19 : in F3A1A00.Untagged_Private;                 -- ERROR:
      O20 : in F3A1A00.Tagged_Private;                   -- ERROR:
   package Gen is
   end Gen;

   type Untagged_Inc is range 1 .. 100;

   type Tagged_Inc is tagged null record;

end B3A1A05;
