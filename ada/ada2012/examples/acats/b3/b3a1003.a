-- B3A1003.A
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
--     Check that an incomplete type cannot be completed by another
--     incomplete type declaration or a subtype declaration.
--
-- TEST DESCRIPTION:
--
--     Test both normal and tagged incomplete types. AI05-0162-1 changes
--     the language so that incomplete types can be completed by private
--     types, so those cases are no longer errors. They've been left in
--     this test to check that they're in fact allowed.
--
-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setnn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--     implementation to pass.
--
-- CHANGE HISTORY:
--     29 May 2008  RLB  Created test based on test B3A1001.
--     18 Jul 2008  RLB  Changed error locations slightly.
--     12 Mar 2014  RLB  Modified test and objective to reflect AI05-0162-1,
--                       many previous errors are now OK.
--
procedure B3A1003 is

   type A_Tagged is tagged null record;

   package Pack1 is
      type T1;                               -- POSSIBLE ERROR: [Set01]
      type TT1 is tagged;                    -- POSSIBLE ERROR: [Set02]
      type T1;                               -- POSSIBLE ERROR: [Set01]
      type TT1 is tagged;                    -- POSSIBLE ERROR: [Set02]
      type T1 is (Red, Green, Blue);
      type TT1 is tagged null record;
   end Pack1;

   package Pack2 is
      type T2;
      type TT2 is tagged;
      type T2 is private;                    -- OK. (Only for Ada 2012)
      type TT2 is tagged private;            -- OK. (Only for Ada 2012)
   private
      type T2 is null record;
      type TT2 is tagged null record;
   end Pack2;

   package Pack3 is
      type T3;
      type TT3 is tagged;
      type T3 is new A_Tagged with private;  -- OK. (Only for Ada 2012)
      type TT3 is new A_Tagged with private; -- OK. (Only for Ada 2012)
   private
      type T3 is new A_Tagged with null record;
      type TT3 is new A_Tagged with null record;
   end Pack3;

   package Pack4 is
      type T4;                               -- POSSIBLE ERROR: [Set03]
      type TT4 is tagged;                    -- POSSIBLE ERROR: [Set04]
      subtype T4 is A_Tagged;                -- POSSIBLE ERROR: [Set03]
      subtype TT4 is A_Tagged;               -- POSSIBLE ERROR: [Set04]
   end Pack4;

   package Pack5 is
   private
      type T5;                               -- OK.
      type TT5 is tagged;                    -- OK.
   end Pack5;

   package body Pack5 is
      type T5;                               -- ERROR:
      type TT5 is tagged;                    -- ERROR:
      type T5 is (Red, Green, Blue);
      type TT5 is tagged null record;
   end Pack5;

   package Pack6 is
   private
      type T6;                               -- POSSIBLE ERROR: [Set05]
      type TT6 is tagged;                    -- POSSIBLE ERROR: [Set06]
   end Pack6;

   package body Pack6 is
      subtype T6 is A_Tagged;                -- POSSIBLE ERROR: [Set05]
      subtype TT6 is A_Tagged;               -- POSSIBLE ERROR: [Set06]
   end Pack6;

begin
    null;
end B3A1003;
