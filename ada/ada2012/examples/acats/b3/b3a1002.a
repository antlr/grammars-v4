-- B3A1002.A
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
-- (1) Check that an incomplete type is illegal if there is no full type
--     that completes it.
-- (2) Check that an incomplete type given in the visible part of a
--     package cannot be completed in the private part or body of the package.
-- (3) Check that an incomplete type given in a declarative part or package
--     cannot be completed in a more nested declarative part or package.
--
-- TEST DESCRIPTION:
--
--     Part B: Test generic package specifications and generic package bodies.
--             Test both normal and tagged incomplete types.
--
-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setnn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--     implementation to pass.
--
-- CHANGE HISTORY:
--     29 May 2008  RLB  Created test based on old ACATS test B38101C
--                       and new ACATS test B3A1001.
--     15 Mar 2014  RLB  Corrected type UT1 to be tagged.
--     23 Jun 2016  RLB  Split Pack6 case so error sets are unique.

procedure B3A1002 is

   generic
   package Pack1 is
      type T1;                          -- ERROR: (1) for T1.
      type TT1 is tagged;               -- ERROR: (1) for TT1.
   end Pack1;

   generic
   package Pack1A is
   private
      type U1;                          -- POSSIBLE ERROR: [Set01] (1) for U1.
   end Pack1A;

   generic
   package Pack1B is
   private
      type UT1 is tagged;               -- POSSIBLE ERROR: [Set02] (1) for UT1.
   end Pack1B;

   generic
   package Pack2 is
      type T2;                          -- POSSIBLE ERROR: [Set03] (2) for T2.
      type TT2 is tagged;               -- POSSIBLE ERROR: [Set04] (2) for TT2.
   private
      type T2 is (X);                   -- POSSIBLE ERROR: [Set03] (2) for T2.
      type TT2 is tagged null record;   -- POSSIBLE ERROR: [Set04] (2) for TT2.
   end Pack2;

   generic
   package Pack3 is
      type T3;                          -- POSSIBLE ERROR: [Set05] (2) for T3.
      type TT3 is tagged;               -- POSSIBLE ERROR: [Set06] (2) for TT3.
   end Pack3;

   generic
   package Pack4 is
   private
      type U3;                          -- OK.
      type UT3 is tagged;               -- OK.
   end Pack4;

   package body Pack1A is
   end;                                 -- POSSIBLE ERROR: [Set01] (1) for U1.

   package body Pack1B is
   end;                                 -- POSSIBLE ERROR: [Set02] (1) for UT1.

   package body Pack3 is
      type T3 is null record;           -- POSSIBLE ERROR: [Set05] (2) for T3.
      type TT3 is tagged null record;   -- POSSIBLE ERROR: [Set06] (2) for TT3.
   end Pack3;

   package body Pack4 is
      type U3 is array (1..10) of Boolean;-- OK U3.
      type UT3 is tagged null record;   -- OK UT3.
      type T4;                          -- POSSIBLE ERROR: [Set07] (3) for T4.
      type TT4 is tagged;               -- POSSIBLE ERROR: [Set08] (3) for TT4.
      type U4;                          -- POSSIBLE ERROR: [Set09] (3) for U4.
      type UT4 is tagged;               -- POSSIBLE ERROR: [Set10] (3) for UT4.
      type W4;                          -- POSSIBLE ERROR: [Set11] (3) for W4.
      type WT4 is tagged;               -- POSSIBLE ERROR: [Set12] (3) for WT4.
      type X4;                          -- POSSIBLE ERROR: [Set13] (3) for X4.
      type XT4 is tagged;               -- POSSIBLE ERROR: [Set14] (3) for XT4.
      type Y4;                          -- POSSIBLE ERROR: [Set15] (3) for Y4.
      type YT4 is tagged;               -- POSSIBLE ERROR: [Set16] (3) for YT4.
      generic
      package Pack4A is
         type T4 is null record;        -- POSSIBLE ERROR: [Set07] (3) for T4.
         type TT4 is tagged null record;-- POSSIBLE ERROR: [Set08] (3) for TT4.
      end Pack4A;
      generic
      package Pack4B is
      end Pack4B;
      package body Pack4B is
         type U4 is null record;        -- POSSIBLE ERROR: [Set09] (3) for U4.
         type UT4 is null record;       -- POSSIBLE ERROR: [Set10] (3) for UT4.
      begin
         null;
      end Pack4B;
      procedure Proc1 is
         type X4 is null record;        -- POSSIBLE ERROR: [Set13] (3) for X4.
         type XT4 is tagged null record;-- POSSIBLE ERROR: [Set14] (3) for XT4.
      begin
         declare
            type W4 is null record;     -- POSSIBLE ERROR: [Set11] (3) for W4.
            type WT4 is tagged null record;-- POSSIBLE ERROR: [Set12] (3) WT4.
         begin
            null;
         end;
      end Proc1;
      task type Tsk1;
      task body Tsk1 is
         type Y4 is null record;        -- POSSIBLE ERROR: [Set15] (3) for Y4.
         type YT4 is tagged null record;-- POSSIBLE ERROR: [Set16] (3) for YT4.
      begin
         null;
      end Tsk1;
   end Pack4;

   generic
   package Pack5 is
      type T5;                          -- POSSIBLE ERROR: [Set17] (3) for T5.
      type TT5 is tagged;               -- POSSIBLE ERROR: [Set18] (3) for TT5.
      package Pack5A is
         type T5 is (X);                -- POSSIBLE ERROR: [Set17] (3) for T5.
         type TT5 is tagged null record;-- POSSIBLE ERROR: [Set18] (3) for TT5.
      end Pack5A;
   end Pack5;

   generic
   package Pack6 is
   private
      type U6;                          -- POSSIBLE ERROR: [Set19] (3) for U6.
      package Pack6A is
         type U6 is null record;        -- POSSIBLE ERROR: [Set19] (3) for U6.
      end Pack6A;
   end Pack6;

   package body Pack6 is
   end Pack6;                           -- POSSIBLE ERROR: [Set19] (3) for U6.

   generic
   package Pack7 is
   private
      type UT7 is tagged;               -- POSSIBLE ERROR: [Set20] (3) for UT7.
      package Pack7A is
         type UT7 is tagged null record;-- POSSIBLE ERROR: [Set20] (3) for UT7.
      end Pack7A;
   end Pack7;

   package body Pack7 is
   end Pack7;                           -- POSSIBLE ERROR: [Set20] (3) for UT7.

begin
   null;
end B3A1002;
