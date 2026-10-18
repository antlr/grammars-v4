-- B730011.A

--                             Grant of Unlimited Rights
--
--     Under contracts F33600-87-D-0337, F33600-84-D-0280, MDA903-79-C-0687,
--     F08630-91-C-0015, and DCA100-97-D-0025, the U.S. Government obtained
--     unlimited rights in the software and documentation contained herein.
--     Unlimited rights are defined in DFAR 252.227-7013(a)(19).  By making
--     this public release, the Government intends to confer upon all
--     recipients unlimited rights  equal to those held by the Government.
--     These rights include rights to use, duplicate, release or disclose the
--     released technical data and computer software in whole or in part, in
--     any manner and for any purpose whatsoever, and to have or permit others
--     to do so.
--
--                                    DISCLAIMER
--
--     ALL MATERIALS OR INFORMATION HEREIN RELEASED, MADE AVAILABLE OR
--     DISCLOSED ARE AS IS.  THE GOVERNMENT MAKES NO EXPRESS OR IMPLIED
--     WARRANTY AS TO ANY MATTER WHATSOEVER, INCLUDING THE CONDITIONS OF THE
--     SOFTWARE, DOCUMENTATION OR OTHER INFORMATION RELEASED, MADE AVAILABLE
--     OR DISCLOSED, OR THE OWNERSHIP, MERCHANTABILITY, OR FITNESS FOR A
--     PARTICULAR PURPOSE OF SAID MATERIAL.
--*
--
-- OBJECTIVE:
--    Check that a private type or private extension cannot appear in
--    the private part of a package specification, in a package body,
--    a subprogram, a block, or a task body.
--
--    Check that the full definition of a private type or private
--    extension cannot appear in the visible part, and cannot be
--    omitted from the package specification (even if it is provided
--    in the package body).
--
--    Check that the full declaration of a private type or private extension
--    cannot appear in the private part of a nested or enclosing package
--    specification.
--
-- TEST DESCRIPTION:
--    This test is based on the early Legacy test B74001A. However, Ada 2005
--    has 6 kinds of private types along with 6 kinds of private extensions,
--    all of which are subject to the rules of 7.3(4), from which these
--    objectives are derived. We try a few examples of each kind of private
--    type and private extension (rather than trying to be exhaustive as the
--    legacy test was).
--
--    We also reorganize the test cases so that the errors are not intertwined,
--    which makes grading unnecessarily difficult.
--
--    We don't have to add protected declarations and bodies to these
--    objectives, as those don't allow any kind of type declaration (which
--    is, of course, a separate objective).
--
--    6 kinds of private type:
--       private
--       limited private
--       tagged private
--       tagged limited private
--       abstract tagged private
--       abstract tagged limited private
--    6 kinds of private extension:
--       new <ancestor> with private
--       limited new <ancestor> with private
--       synchronized new <ancestor> with private
--       abstract new <ancestor> with private
--       abstract limited new <ancestor> with private
--       abstract synchronized new <ancestor> with private
--
-- PASS/FAIL CRITERIA:
--    The test contains several lines marked POSSIBLE ERROR: [Setnn].
--    For each value of nn, the implementation must detect one or more of
--    these possible errors. For instance, an error must be detected on
--    at least one of the lines labeled POSSIBLE ERROR: [Set01] for an
--    implementation to pass.
--
-- CHANGE HISTORY:
--     03 Apr 1981   DAT
--     26 Mar 1984   JRK
--     06 Feb 2017   RLB   Created test from legacy test. Used additional
--                         kinds of private types and private extensions.
--                         Split some test parts so that the errors are
--                         separated.
--
--!

procedure B730011 is

   type Root is tagged record
      B : Boolean;
   end record;

   type Lim_Root is tagged limited record
      F : Float;
   end record;

   type Lim_Intf is limited interface;

   package P1 is
      type T5 is private;
      type T6 is limited private;            -- POSSIBLE ERROR: [Set01] {7}

      package P2 is
         type T9 is tagged private;          -- POSSIBLE ERROR: [Set02] {10}
      private
         type T7 is range 1 .. 2;
      end P2;                                -- POSSIBLE ERROR: [Set02] {1:33}
      use P2;                                -- Undefined T9.

      package P3 is
         type TA is tagged limited private;  -- POSSIBLE ERROR: [Set03] {10}
      private
         type T6 is range 1 .. 2;
      end P3;                                -- POSSIBLE ERROR: [Set03] {1:33}
      use P3;                                -- Undefined TA.

      type T5 is range 1 .. 2;               -- ERROR: {7}
                                             -- Completion in visible part.
   private
      type TB is abstract tagged
                        limited private;     -- ERROR: {1:7} in private part.
      type TC is private;                    -- ERROR: {7} in private part.
      type T9 is new Root with null record;
      type TA is limited new Lim_Root with null record;
   end P1;                                   -- POSSIBLE ERROR: [Set01] {1:55}
                                             -- Undefined T6.

   package P2 is
      type T7 is new Root with private;
      type T8 is limited new Lim_Root
                                with private;-- POSSIBLE ERROR: [Set04] {1:7}

      package P3 is
         type T4 is abstract tagged private; -- POSSIBLE ERROR: [Set05] {10}
      private
         type T7 is new Root with null record;
      end P3;                                -- POSSIBLE ERROR: [Set05] {1:46}
      use P3;                                -- Undefined T4.

      package P4 is
         type T3 is abstract new Root
                          with private;      -- POSSIBLE ERROR: [Set06] {1:10}
      private
         type T8 is limited new Lim_Root with null record;
      end P4;                                -- POSSIBLE ERROR: [Set06] {1:58}
      use P4;                                -- Undefined T3.

      type T7 is new Root with null record;  -- ERROR: {7}
                                             -- Completion in visible part.
   private
      type TB is abstract synchronized
                new Lim_Intf with private;   -- ERROR: {1:7} in private part.
      type TC is limited
                new Lim_Root with private;   -- ERROR: {1:7} in private part.
      type T4 is new Root with null record;
      type T3 is new Root with null record;
   end P2;                                   -- POSSIBLE ERROR: [Set04] {1:44}
                                             -- Undefined T7.

   package P3 is
      type T1 is tagged private;             -- POSSIBLE ERROR: [Set07] {7}
   private
      I : Integer;
   end P3;                                   -- POSSIBLE ERROR: [Set07] {1:18}
                                             -- Undefined T1.

   package P4 is
      type T2 is abstract limited new Lim_Root
                               with private; -- POSSIBLE ERROR: [Set08] {1:7}
   private
      I : Integer;
   end P4;                                   -- POSSIBLE ERROR: [Set08] {1:18}
                                             -- Undefined T2.

   package P5 is
      type T3 is limited private;
   private
   end P5;                                   -- ERROR: {2:7} Undefined T3.

   package P6 is
      type T4 is new Root with private;
   private
   end P6;                                   -- ERROR: {2:7} Undefined T4.

   package P7 is
      C : Character;
   end P7;

   task Tsk;

   package body P7 is
      type XX is (X);                        -- OK.
      type TD is private;                    -- ERROR: {7} in body.
      type TE is limited private;            -- ERROR: {7} in body.
      type TF is tagged private;             -- ERROR: {7} in body.
      type TG is tagged limited private;     -- ERROR: {7} in body.
      type TH is abstract tagged private;    -- ERROR: {7} in body.
      type TI is abstract tagged
                            limited private; -- ERROR: {1:7} in body.
      type TJ is new Root with private;      -- ERROR: {7} in body.
      type TK is limited
                new Lim_Root with private;   -- ERROR: {1:7} in body.
      type TL is synchronized
                new Lim_Intf with private;   -- ERROR: {1:7} in body.
      type TM is abstract
                    new Root with private;   -- ERROR: {1:7} in body.
      type TN is abstract limited
                new Lim_Root with private;   -- ERROR: {1:7} in body.
      type TQ is abstract synchronized
                new Lim_Intf with private;   -- ERROR: {1:7} in body.
   end P7;

   procedure PR is
      type TR is private;                    -- ERROR: {7} in procedure.
      type TS is tagged limited private;     -- ERROR: {7} in procedure.
      type TT is new Root with private;      -- ERROR: {7} in procedure.
      type TU is abstract limited
                new Lim_Root with private;   -- ERROR: {1:7} in procedure.
   begin
      null;
   end PR;

   function F return Boolean is
      type T1 is limited private;            -- ERROR: {7} in function.
      type T2 is tagged private;             -- ERROR: {7} in function.
      type T3 is limited new Lim_Root
                          with private;      -- ERROR: {1:7} in function.
      type T4 is abstract
                    new Root with private;   -- ERROR: {1:7} in function.
   begin
      return True;
   end F;

   task body Tsk is
      type T5 is abstract tagged private;    -- ERROR: {7} in task body.
      type T6 is private;                    -- ERROR: {7} in task body.
      type T7 is synchronized new Lim_Intf
                          with private;      -- ERROR: {1:7} in task body.
      type T8 is new Lim_Root with private;  -- ERROR: {7} in task body.
   begin
      null;
   end Tsk;

begin
   declare
      type TA is tagged private;             -- ERROR: {7} in block.
      type TB is abstract tagged
                        limited private;     -- ERROR: {1:7} in block.
      type TC is limited new Lim_Root
                          with private;      -- ERROR: {1:7} in block.
      type TD is abstract
                    new Root with private;   -- ERROR: {1:7} in block.
   begin
      null;
   end;
end B730011;
