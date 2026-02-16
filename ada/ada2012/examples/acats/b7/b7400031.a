-- B7400031.A
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
--    The ACAA has created and maintains the Ada Conformity Assessment Test
--    Suite for the purpose of conformity assessments conducted in accordance
--    with the International Standard ISO/IEC 18009 - Ada: Conformity
--    assessment of a language processor. This test suite should not be used
--    to make claims of conformance unless used in accordance with
--    ISO/IEC 18009 and any applicable ACAA procedures.
--*
--
-- OBJECTIVE:
--     See B7400030.A.
--
-- TEST DESCRIPTION
--     See B7400030.A.
--
-- TEST FILES:
--     This test consists of the following files:
--         B7400030.A
--      -> B7400031.A
--         B7400032.AM
--
-- PASS/FAIL CRITERIA:
--     See B7400030.A.
--
-- CHANGE HISTORY:
--      30 Apr 07   RLB   Created from ACATS 2.6 test.
--      17 Aug 07   RLB   Moved error indications based on implementer
--                        comments.
--      29 May 08   RLB   Added more flexibility in error indications.
--      13 Jun 18   RLB   Added error location indicators.
--!

package B7400031 is

   type Priv_Comp is private;

   type List_Of_Priv_Comp is array (Positive range <>) of Priv_Comp;

   Null_List_OK : constant List_Of_Priv_Comp; -- OK. {4;1}

   type Tagged_Type is tagged record
      C : Priv_Comp;
   end record;

   Tagged_OK : constant Tagged_Type'Class;    -- OK. {4;1}
   Tagged_NO : constant Tagged_Type'Class;    -- POSSIBLE ERROR: [Set5] {4;1}
            -- Deferred constant completed in wrong scope.

   type Rec is record
      C : Integer;
   end record;

   Rec_OK : constant Rec;                     -- OK. {4;1}

   Rec_NO : constant Rec;                     -- POSSIBLE ERROR: [Set6] {4;1}
                       -- Deferred constant completed in visible part.

   Rec_NO : constant Rec := (C => 13);        -- POSSIBLE ERROR: [Set6] {4;1}
                       -- Deferred constant completed in visible part.

   subtype Short_Int is Integer range 1 .. 512;
   type Dis_Rec (Disc : Short_Int := 100) is
      record
         C : String (1 .. Disc);
      end record;

   Constr_Rec : constant Dis_Rec;             -- POSSIBLE ERROR: [Set7] {4;1}
            -- Deferred constant completed in wrong scope.

   package A_Nested is
      ID : constant String (1 .. 12);        -- POSSIBLE ERROR: [Set8] {7;1}
             -- Deferred constant not completed in nested package.
   private
      Constr_Rec : constant Dis_Rec :=
           (Disc => 19,
            C => "Constraints Allowed");     -- POSSIBLE ERROR: [Set7] {2:7;1}
             -- Deferred constant completed in private part of nested package.
   end A_Nested;

private

   type Priv_Comp is (Anything);      -- Enumeration type.

   Null_List_OK : constant List_Of_Priv_Comp := (1 .. 0 => Anything);

   Null_List_NO : constant List_Of_Priv_Comp;-- POSSIBLE ERROR: [Set9] {4;1}
                                       -- Deferred constant in private part.
   Null_List_NO : constant List_Of_Priv_Comp :=
         (1 .. 0 => Anything);               -- POSSIBLE ERROR: [Set9] {1:4;1}
                                       -- Deferred constant in private part.


   Tagged_OK : constant Tagged_Type'Class := Tagged_Type'(C => Anything);

   package Another_Nested is
      Tagged_NO : constant Tagged_Type'Class :=
         Tagged_Type'(C => Anything);        -- POSSIBLE ERROR: [Set5] {1:7;1}
                            -- Deferred constant completed in nested package.
   end Another_Nested;

   Rec_OK : constant Rec := (C => 12);

   ID : constant String (1 .. 12) :=
                             "YourNameHere"; -- POSSIBLE ERROR: [Set8] {1:4;1}
                            -- Deferred constant completed in outer package.

end B7400031;

