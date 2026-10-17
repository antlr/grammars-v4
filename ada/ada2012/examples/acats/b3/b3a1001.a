-- B3A1001.A
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
--     Part A: Test normal package specifications, normal package bodies,
--             blocks, subprograms, and task bodies. Test both normal and
--             tagged incomplete types.
--
-- PASS/FAIL CRITERIA:
--     The test contains several lines marked POSSIBLE ERROR: [Setnn].
--     For each value of n, the implementation must detect one or more of
--     these possible errors. For instance, an error must be detected on
--     at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--     implementation to pass.
--
-- CHANGE HISTORY:
--     16 Jul 1981  ASL
--     21 Jan 1983  VKG
--     26 Aug 1986  AH   Modified comments.
--     29 May 2008  RLB  Created test based on old ACATS test B38101A (for
--                       which the previous change entries apply).
--     15 Mar 2014  RLB  Corrected type UT1 to be tagged.
--     23 Jun 2016  RLB  Split Pack6 case so error sets are unique.

procedure B3A1001 is

   package Pack1 is
      type T1;                          -- ERROR: (1) for T1.
      type TT1 is tagged;               -- ERROR: (1) for TT1.
   end Pack1;

   package Pack1A is
   private
      type U1;                          -- POSSIBLE ERROR: [Set01] (1) for U1.
   end Pack1A;

   package Pack1B is
   private
      type UT1 is tagged;               -- POSSIBLE ERROR: [Set02] (1) for UT1.
   end Pack1B;

   package Pack2 is
      type T2;                          -- POSSIBLE ERROR: [Set03] (2) for T2.
      type TT2 is tagged;               -- POSSIBLE ERROR: [Set04] (2) for TT2.
   private
      type T2 is (X);                   -- POSSIBLE ERROR: [Set03] (2) for T2.
      type TT2 is tagged null record;   -- POSSIBLE ERROR: [Set04] (2) for TT2.
   end Pack2;

   package Pack3 is
      type T3;                          -- POSSIBLE ERROR: [Set05] (2) for T3.
      type TT3 is tagged;               -- POSSIBLE ERROR: [Set06] (2) for TT3.
   end Pack3;

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
      package Pack4A is
         type T4 is null record;        -- POSSIBLE ERROR: [Set07] (3) for T4.
         type TT4 is tagged null record;-- POSSIBLE ERROR: [Set08] (3) for TT4.
      end Pack4A;
      package Pack4B is
      end Pack4B;
      package body Pack4B is
         type U4 is null record;        -- POSSIBLE ERROR: [Set09] (3) for U4.
         type UT4 is null record;       -- POSSIBLE ERROR: [Set10] (3) for UT4.
      begin
         declare
            type W4 is null record;     -- POSSIBLE ERROR: [Set11] (3) for W4.
            type WT4 is tagged null record;-- POSSIBLE ERROR: [Set12] (3) WT4.
         begin
            null;
         end;
      end Pack4B;
      procedure Proc1 is
         type X4 is null record;        -- POSSIBLE ERROR: [Set13] (3) for X4.
         type XT4 is tagged null record;-- POSSIBLE ERROR: [Set14] (3) for XT4.
      begin
         null;
      end Proc1;
      task type Tsk1;
      task body Tsk1 is
         type Y4 is null record;        -- POSSIBLE ERROR: [Set15] (3) for Y4.
         type YT4 is tagged null record;-- POSSIBLE ERROR: [Set16] (3) for YT4.
      begin
         null;
      end Tsk1;
   end Pack4;

   package Pack5 is
      type T5;                          -- POSSIBLE ERROR: [Set17] (3) for T5.
      type TT5 is tagged;               -- POSSIBLE ERROR: [Set18] (3) for TT5.
      package Pack5A is
         type T5 is (X);                -- POSSIBLE ERROR: [Set17] (3) for T5.
         type TT5 is tagged null record;-- POSSIBLE ERROR: [Set18] (3) for TT5.
      end Pack5A;
   end Pack5;

   package Pack6A is
   private
      type U6;                          -- POSSIBLE ERROR: [Set19] (3) for U6.
      package Pack6B is
         type U6 is null record;        -- POSSIBLE ERROR: [Set19] (3) for U6.
      end Pack6B;
   end Pack6A;

   package body Pack6A is
   end Pack6A;                          -- POSSIBLE ERROR: [Set19] (3) for U6.

   package Pack6C is
   private
      type UT6 is tagged;               -- POSSIBLE ERROR: [Set20] (3) for UT6.
      package Pack6D is
         type UT6 is tagged null record;-- POSSIBLE ERROR: [Set20] (3) for UT6.
      end Pack6D;
   end Pack6C;

   package body Pack6C is
   end Pack6C;                          -- POSSIBLE ERROR: [Set20] (3) for UT6.

   procedure PROC2 is
      type Z7;                          -- ERROR: (1) for Z7.
      type ZT7 is tagged;               -- ERROR: (1) for ZT7.
      type T7;                          -- POSSIBLE ERROR: [Set21] (3) for T7.
      type TT7 is tagged;               -- POSSIBLE ERROR: [Set22] (3) for TT7.
      type U7;                          -- POSSIBLE ERROR: [Set23] (3) for U7.
      type UT7 is tagged;               -- POSSIBLE ERROR: [Set24] (3) for UT7.
      type W7;                          -- POSSIBLE ERROR: [Set25] (3) for W7.
      type WT7 is tagged;               -- POSSIBLE ERROR: [Set26] (3) for WT7.
      type X7;                          -- POSSIBLE ERROR: [Set27] (3) for X7.
      type XT7 is tagged;               -- POSSIBLE ERROR: [Set28] (3) for XT7.
      type Y7;                          -- POSSIBLE ERROR: [Set29] (3) for Y7.
      type YT7 is tagged;               -- POSSIBLE ERROR: [Set30] (3) for YT7.
      package Pack7A is
         type T7 is null record;        -- POSSIBLE ERROR: [Set21] (3) for T7.
         type TT7 is tagged null record;-- POSSIBLE ERROR: [Set22] (3) for TT7.
      end Pack7A;
      package Pack7B is
      end Pack7B;
      package body Pack7B is
         type U7 is null record;        -- POSSIBLE ERROR: [Set23] (3) for U7.
         type UT7 is null record;       -- POSSIBLE ERROR: [Set24] (3) for UT7.
      begin
         declare
            type W7 is null record;     -- POSSIBLE ERROR: [Set25] (3) for W7.
            type WT7 is tagged null record;-- POSSIBLE ERROR: [Set26] (3) WT7.
         begin
            null;
         end;
      end Pack7B;
      procedure Proc2A is
         type X7 is null record;        -- POSSIBLE ERROR: [Set27] (3) for X7.
         type XT7 is tagged null record;-- POSSIBLE ERROR: [Set28] (3) for XT7.
      begin
         null;
      end Proc2A;
      task type Tsk2;
      task body Tsk2 is
         type Y7 is null record;        -- POSSIBLE ERROR: [Set29] (3) for Y7.
         type YT7 is tagged null record;-- POSSIBLE ERROR: [Set30] (3) for YT7.
      begin
         null;
      end Tsk2;
   begin
      null;
   end Proc2;

   task type Tsk3;
   task body Tsk3 is
      type Z8;                          -- ERROR: (1) for Z8.
      type ZT8 is tagged;               -- ERROR: (1) for ZT8.
      type T8;                          -- POSSIBLE ERROR: [Set31] (3) for T8.
      type TT8 is tagged;               -- POSSIBLE ERROR: [Set32] (3) for TT8.
      type U8;                          -- POSSIBLE ERROR: [Set33] (3) for U8.
      type UT8 is tagged;               -- POSSIBLE ERROR: [Set34] (3) for UT8.
      type W8;                          -- POSSIBLE ERROR: [Set35] (3) for W8.
      type WT8 is tagged;               -- POSSIBLE ERROR: [Set36] (3) for WT8.
      type X8;                          -- POSSIBLE ERROR: [Set37] (3) for X8.
      type XT8 is tagged;               -- POSSIBLE ERROR: [Set38] (3) for XT8.
      type Y8;                          -- POSSIBLE ERROR: [Set39] (3) for Y8.
      type YT8 is tagged;               -- POSSIBLE ERROR: [Set40] (3) for YT8.
      package Pack8A is
         type T8 is null record;        -- POSSIBLE ERROR: [Set31] (3) for T8.
         type TT8 is tagged null record;-- POSSIBLE ERROR: [Set32] (3) for TT8.
      end Pack8A;
      package Pack8B is
      end Pack8B;
      package body Pack8B is
         type U8 is null record;        -- POSSIBLE ERROR: [Set33] (3) for U8.
         type UT8 is null record;       -- POSSIBLE ERROR: [Set34] (3) for UT8.
      begin
         declare
            type W8 is null record;     -- POSSIBLE ERROR: [Set35] (3) for W8.
            type WT8 is tagged null record;-- POSSIBLE ERROR: [Set36] (3) WT8.
         begin
            null;
         end;
      end Pack8B;
      procedure Proc3 is
         type X8 is null record;        -- POSSIBLE ERROR: [Set37] (3) for X8.
         type XT8 is tagged null record;-- POSSIBLE ERROR: [Set38] (3) XT8.
      begin
         null;
      end Proc3;
      task type Tsk3A;
      task body Tsk3A is
         type Y8 is null record;        -- POSSIBLE ERROR: [Set39] (3) for Y8.
         type YT8 is tagged null record;-- POSSIBLE ERROR: [Set40] (3) YT8.
      begin
         null;
      end Tsk3A;
   begin
      null;
   end Tsk3;

begin

   declare
      type Z9;                          -- ERROR: (1) for Z9.
      type ZT9 is tagged;               -- ERROR: (1) for ZT9.
      type T9;                          -- POSSIBLE ERROR: [Set41] (3) for T9.
      type TT9 is tagged;               -- POSSIBLE ERROR: [Set42] (3) for TT9.
      type U9;                          -- POSSIBLE ERROR: [Set43] (3) for U9.
      type UT9 is tagged;               -- POSSIBLE ERROR: [Set44] (3) for UT9.
      type W9;                          -- POSSIBLE ERROR: [Set45] (3) for W9.
      type WT9 is tagged;               -- POSSIBLE ERROR: [Set46] (3) for WT9.
      type X9;                          -- POSSIBLE ERROR: [Set47] (3) for X9.
      type XT9 is tagged;               -- POSSIBLE ERROR: [Set48] (3) for XT9.
      type Y9;                          -- POSSIBLE ERROR: [Set49] (3) for Y9.
      type YT9 is tagged;               -- POSSIBLE ERROR: [Set50] (3) for YT9.
      package Pack9A is
         type T9 is null record;        -- POSSIBLE ERROR: [Set41] (3) for T9.
         type TT9 is tagged null record;-- POSSIBLE ERROR: [Set42] (3) for TT9.
      end Pack9A;
      package Pack9B is
      end Pack9B;
      package body Pack9B is
         type U9 is null record;        -- POSSIBLE ERROR: [Set43] (3) for U9.
         type UT9 is null record;       -- POSSIBLE ERROR: [Set44] (3) for UT9.
      begin
         declare
            type W9 is null record;     -- POSSIBLE ERROR: [Set45] (3) for W9.
            type WT9 is tagged null record;-- POSSIBLE ERROR: [Set46] (3) WT9.
         begin
            null;
         end;
      end Pack9B;
      procedure Proc4 is
         type X9 is null record;        -- POSSIBLE ERROR: [Set47] (3) for X9.
         type XT9 is tagged null record;-- POSSIBLE ERROR: [Set48] (3) for XT9.
      begin
         null;
      end Proc4;
      task type Tsk4;
      task body Tsk4 is
         type Y9 is null record;        -- POSSIBLE ERROR: [Set49] (3) for Y9.
         type YT9 is tagged null record;-- POSSIBLE ERROR: [Set50] (3) for YT9.
      begin
         null;
      end Tsk4;
   begin
      null;
   end;

end B3A1001;
