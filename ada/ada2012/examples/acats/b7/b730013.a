-- B730013.A

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
-- OBJECTIVE:
--     Check that no basic operations that depend on the full declaration
--     of the type are available for private and limited private types
--     completed by fixed and floating point types.
--
-- TEST DESCRIPTION:
--    This test is based on the Legacy test B74203E. However, that test used
--    a number of attributes not defined in Ada 95, so most of the test was
--    not testing the objective. A number of important cases were not tested,
--    either.
--
-- CHANGE HISTORY:
--    10 Apr 1990  BCB  Create original test from split of B74203B.ADA.
--    20 Mar 2017  RLB  Reformatted test, added/removed test cases, including
--                      replacing attributes dropped by Ada 95 by new Ada 95
--                      attributes.

procedure B730013 is

     package P is
          type Flt is private;
          type Fix is private;
          type LFlt is limited private;
          type LFix is limited private;

          Cons3 : constant Flt;

          Cons4 : constant Fix;

          Cons5 : constant LFlt;

          Cons6 : constant LFix;

          procedure Init_LFlt (One : in out LFlt; Two : LFlt);

          procedure Init_LFix (One : in out LFix; Two : LFix);
     private
          type Flt is digits 5 range -100.0 .. 100.0;

          type Fix is delta 2.0**(-1) range -100.0 .. 100.0;

          type LFlt is digits 5 range -50.0 .. 50.0;

          type LFix is delta 0.25 range -40.0 .. 40.0;

          Cons3 : constant Flt := 1.0;

          Cons4 : constant Fix := 1.0;

          Cons5 : constant LFlt := 1.0;

          Cons6 : constant LFix := 1.0;
     end P;

     use P;

     Bool : Boolean := False;
     Val  : Integer := 0;
     FVal : Float   := 0.0;

     M1 : Flt;

     N1 : Fix;

     M2 : LFlt;

     N2 : LFix;

     package body P is
          procedure Init_LFlt (One : in out LFlt; Two : LFlt) is
          begin
               One := Two;
          end Init_LFlt;

          procedure Init_LFix (One : in out LFix; Two : LFix) is
          begin
               One := Two;
          end Init_LFix;
     end P;

begin

     M1 := Cons3;                   -- OK. {6;1}

     if Flt (FVal) in Flt then      -- ERROR: Conversion. {9;5}
          null;
     end if;

     M1 := 1.0;                     -- ERROR: Literal. {6;1}

     M1 := Cons3 * 2.0;             -- ERROR: * Operator. {6;1}

     Val := Flt'Digits;             -- ERROR: Attribute not defined. {13;1}

     Val := Flt'Model_Mantissa;     -- ERROR: Attribute not defined. {13;1}

     FVal := Flt'Model_Epsilon;     -- ERROR: Attribute not defined. {14;1}

     Val := Flt'Model_EMin;         -- ERROR: Attribute not defined. {13;1}

     FVal := Flt'Model_Small;       -- ERROR: Attribute not defined. {14;1}

     Bool := Flt'Machine_Rounds;    -- ERROR: Attribute not defined. {14;1}

     Bool := Flt'Machine_Overflows; -- ERROR: Attribute not defined. {14;1}

     Val := Flt'Machine_Radix;      -- ERROR: Attribute not defined. {13;1}

     Val := Flt'Machine_Mantissa;   -- ERROR: Attribute not defined. {13;1}

     Val := Flt'Machine_EMax;       -- ERROR: Attribute not defined. {13;1}

     Val := Flt'Machine_EMin;       -- ERROR: Attribute not defined. {13;1}

     Init_LFlt (M2, Cons5);         -- OK. {6;1}

     M2 := Cons5;                   -- ERROR: Assignment. {6;1}

     Val := LFlt'Digits;            -- ERROR: Attribute not defined. {13;1}

     if LFlt'Last in LFlt then      -- ERROR: Attribute not defined. {9;5}
          null;
     elsif LFlt'First in LFlt then  -- ERROR: Attribute not defined. {12;5}
          null;
     elsif LFlt (1.0) in LFlt then  -- ERROR: Conversion. {12;5}
          null;
     end if;

     N1 := Cons4;                   -- OK. {6;1}


     if Fix (FVal) in Fix then      -- ERROR: Conversion. {9;5}
          null;
     end if;

     N1 := 1.0;                     -- ERROR: Literal. {6;1}

     N1 := Cons6 * 4;               -- ERROR: * Operator. {6;1}

     FVal := Fix'Delta;             -- ERROR: Attribute not defined. {14;1}

     Val := Fix'Fore;               -- ERROR: Attribute not defined. {13;1}

     Val := Fix'Aft;                -- ERROR: Attribute not defined. {13;1}

     Init_LFix (N2, Cons6);         -- OK. {6;1}

     FVal := LFix'Delta;            -- ERROR: Attribute not defined. {13;1}

     if LFix'Last in LFix then      -- ERROR: Attribute not defined. {9;5}
          null;
     elsif LFix'First in LFix then  -- ERROR: Attribute not defined. {12;5}
          null;
     elsif LFix (1.0) in LFix then  -- ERROR: Conversion. {12;5}
          null;
     end if;

end B730013;
