-- B851005.A
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
-- OBJECTIVES:
--     Check that an object renaming declaration is illegal if it
--     renames a discriminant-dependent component or subcomponent
--     if the enclosing record type has an unconstrained nominal
--     subtype and the object is not known to be constrained.
--     Part A: "Normal" cases (no generics or access types).
--
-- TEST DESCRIPTION:
--     We started with the outline of legacy test B85002A, and added additional
--     cases to cover cases of known to be constrained not imagined in 1988.
--
--     Discriminant dependent components can be:
--     (1) A component with a discriminant constraint using a discriminant.
--     (2) A component with an index constraint using a discriminant.
--     (3) A component declared in a variant.
--     We indicate components that are none of these by "Not DDC".
--
--     No enclosing object cannot have an unconstrained-but-definite
--         nominal subtype that's not immutably limited and be:
--     (A) A component of a containing composite type (of any other of these
--         choices) [try both array and record cases];
--     (B) A variable stand-alone object;
--     (C) A formal "in out" or "out" parameter of a subprogram or entry;
--     (D) A generic formal in out parameter (note: the subtype is
--         unconstrained by 12.4(9/2) even if the named subtype is
--         constrained);
--     (E) A nonconstant return object in an extended return statement;
--     (F) A dereference of a general access-to-object type (try an
--         access-to-constant case);
--     (G) A dereference of a pool-specific access-to-object type if the
--         designated type has a constrained partial view;
--     (H) A qualified expression of any of these cases (by AI12-0228-1).
--     Additionally, in a generic body, we assume that a type descended from
--     an untagged generic formal private or derived type is not indefinite
--     and has a constrained partial view. (Thus, if the type is unconstrained,
--     all of the cases noted above are illegal.) Also, formal access types
--     are not pool-specific. We could also check that rechecking of the
--     above is done in generic instances.
--
--     All of the following can be the enclosing object, regardless of the
--     subtype:
--     (R) Objects of constrained subtypes (except generic formal "in out"
--         parameters);
--     (S) Any object of a immutably limited type;
--     (T) Any object of an indefinite type;
--     (U) A dereference of a pool-specific access-to-object type if the
--         designated type does not have a constrained partial view;
--     (V) A formal "in" parameter of a subprogram or entry;
--     (W) A constant return object in an extended return statement;
--     (X) A qualified aggregate;
--     (Y) A function call;
--     (Z) A stand-alone constant.
--     (Recall that the rules need to pass for *all* enclosing objects of the
--     renamed component; these cases don't automatically make the rename legal
--     if there are other enclosing objects as well). We only try a few of
--     these cases; C-Tests ought to be used to test these legal cases.
--
--     This test includes tests for all three kinds of components,
--     and cases B, C, E, R, S, T, V, W, X, Y, and Z combined with
--     modifiers A and H.
--
-- CHANGE HISTORY:
--     10 Mar 1988  JET  Created original test (as B85002A).
--     30 Mar 2017  RLB  Created new test using outline of original.
--     06 Jun 2018  RLB  Finished test.
--     13 Jun 2018  RLB  Fixed Ent3 case as noted by reviewer.

procedure B851005 is

   type R0 (D : Integer) is record
      F : Integer := D;
   end record;

   type A0 is array (Integer range <>) of Integer;

   type Rec (D : Integer := 1) is
      record
         F1 : Integer;
         F2 : R0(D);
         F3 : A0(1 .. D);
         case D is
            when 1 =>
               F4 : Integer range -10 .. 0;
               F5 : A0(1..5);
            when others =>
               F6 : Float;
               F7 : R0(5);
           end case;
      end record;

   R1 : Rec := (D => 1, F1 => 12, F2 => (1,1), F3 => (1 => 1),
                F4 => -10, F5 => (1,2,3,4,5));

   R2 : Rec := (2, 10, (2,2), (1,2), 2.71, (5,5));

   function Exp_R3 return Rec is
            (D => 3, F1 => 4, F2 => (3,3), F3 => (4,5,6),
             F6 => 3.14, F7 => (5,5));

   R4 : Rec(4) := (D => 4, F1 => 52, F2 => (4,4), F3 => (9,8,7,6),
             F6 => 2.818, F7 => (5,5));

   CR1 : constant Rec :=
               (D => 1, F1 => 12, F2 => (1,1), F3 => (1 => 1),
                F4 => -10, F5 => (1,2,3,4,5));


   type Indef_Rec (D : Integer) is
      record
         F1 : Integer;
         F2 : R0(D);
         F3 : A0(1 .. D);
         case D is
            when 1 =>
               F4 : Integer range -10 .. 0;
               F5 : A0(1..5);
            when others =>
               F6 : Float;
               F7 : R0(5);
           end case;
      end record;

   IR3 : Indef_Rec :=
            (D => 3, F1 => 4, F2 => (3,3), F3 => (4,5,6),
             F6 => 3.14, F7 => (5,5));

   type Lim_Rec (D : Integer := 3) is limited record
      F1 : Integer;
      F2 : R0(D);
      F3 : A0(1 .. D);
      case D is
         when 1 =>
            F4 : Integer range -10 .. 0;
            F5 : A0(1..5);
         when others =>
            F6 : Float;
            F7 : R0(5);
      end case;
   end record;

   LR3 : Lim_Rec :=
            (D => 3, F1 => 4, F2 => (3,3), F3 => (4,5,6),
             F6 => 3.14, F7 => (5,5));

   X01 : Integer renames R1.F1;               -- OK. {4;1} (B, not DDC)
   X02 : R0 renames R1.F2;                    -- ERROR: {4;1} (B1)
   X03 : A0 renames R1.F3;                    -- ERROR: {4;1} (B2)
   X04 : Integer renames R1.F4;               -- ERROR: {4;1} (B3)
   X05 : A0 renames R1.F5;                    -- ERROR: {4;1} (B3)
   X06 : Float renames R2.F6;                 -- ERROR: {4;1} (B3)
   X07 : R0 renames R2.F7;                    -- ERROR: {4;1} (B3)
   X08 : R0 renames Exp_R3.F2;                -- OK. {4;1} (Y1)
   X09 : A0 renames Exp_R3.F3;                -- OK. {4;1} (Y2)
   X10 : Float renames Exp_R3.F6;             -- OK. {4;1} (Y3)
   X11 : Float renames Rec'(Exp_R3).F6;       -- OK. {4;1} (YH3)
   X12 : R0 renames CR1.F2;                   -- OK. {4;1} (Z1)
   X13 : A0 renames CR1.F3;                   -- OK. {4;1} (Z2)
   X14 : Integer renames CR1.F4;              -- OK. {4;1} (Z3)
   X15 : Integer renames Rec'(CR1).F4;        -- OK. {4;1} (ZH3)
   X16 : R0 renames IR3.F2;                   -- OK. {4;1} (T1)
   X17 : A0 renames IR3.F3;                   -- OK. {4;1} (T2)
   X18 : R0 renames IR3.F7;                   -- OK. {4;1} (T3)
   X19 : A0 renames Indef_Rec'(IR3).F3;       -- OK. {4;1} (TH2)
   X20 : R0 renames LR3.F2;                   -- OK. {4;1} (S1)
   X21 : A0 renames LR3.F3;                   -- OK. {4;1} (S2)
   X22 : R0 renames LR3.F7;                   -- OK. {4;1} (S3)
   X23 : R0 renames Lim_Rec'(LR3).F2;         -- OK. {4;1} (SH1)
   X24 : Integer renames
    Rec'(2, 10, (2,2), (1,2), 2.71, (5,5)).F1;-- OK. {1:4;1} (X, Not DDC)
   X25 : R0 renames
    Rec'(2, 10, (2,2), (1,2), 2.71, (5,5)).F2;-- OK. {1:4;1} (X1)
   X26 : A0 renames
    Rec'(2, 10, (2,2), (1,2), 2.71, (5,5)).F3;-- OK. {1:4;1} (X1)
   X27 : Float renames
    Rec'(2, 10, (2,2), (1,2), 2.71, (5,5)).F6;-- OK. {1:4;1} (X1)

   X28 : Integer renames Rec'(R1).F1;         -- OK. {4;1} (BH, not DDC)
   X29 : R0 renames Rec'(R1).F2;              -- ERROR: {4;1} (BH1)
   X30 : A0 renames Rec'(R2).F3;              -- ERROR: {4;1} (BH2)
   X31 : Integer renames Rec'(R1).F4;         -- ERROR: {4;1} (BH3)
   X32 : Integer renames R2.F2.F;             -- ERROR: {4;1} (AB1)
   X33 : Integer renames R2.F3(1);            -- ERROR: {4;1} (AB2)
   X34 : Integer renames R2.F7.F;             -- ERROR: {4;1} (AB3)
   X35 : R0 renames R4.F2;                    -- OK. {4;1} (R1)
   X36 : A0 renames R4.F3;                    -- OK. {4;1} (R2)
   X37 : R0 renames R4.F7;                    -- OK. {4;1} (R3)
   X38 : Integer renames R4.F2.F;             -- OK. {4;1} (AR1)
   X39 : Integer renames R4.F3(1);            -- OK. {4;1} (AR2)
   X40 : Integer renames R4.F7.F;             -- OK. {4;1} (AR3)
   X41 : R0 renames Rec'(R4).F2;              -- OK. {4;1} (RH1)
   X42 : A0 renames Rec'(R4).F3;              -- OK. {4;1} (RH2)
   X43 : Integer renames Rec'(R4).F4;         -- OK. {4;1} (RH3)


   type Rec2 is record
      F : Rec;
   end record;

   RR1 : Rec2 := (F => (D => 1, F1 => 12, F2 => (1,1), F3 => (1 => 1),
                        F4 => -10, F5 => (1,2,3,4,5)));
   RR2 : Rec2 := (F => (2, 10, (2,2), (1,2), 2.71, (2,2)));

   type Lim_Rec2 is limited record
      F : Rec;
   end record;

   LRR2 : Lim_Rec2 := (F => (2, 10, (2,2), (1,2), 2.71, (2,2)));

   RX01 : Integer renames RR1.F.F1;           -- OK. {4;1} (Not DDC)
   RX02 : R0 renames RR1.F.F2;                -- ERROR: {4;1} (B1)
   RX03 : A0 renames RR1.F.F3;                -- ERROR: {4;1} (B2)
   RX04 : Integer renames RR1.F.F4;           -- ERROR: {4;1} (B3)
   RX05 : A0 renames RR1.F.F5;                -- ERROR: {4;1} (B3)
   RX06 : Float renames RR2.F.F6;             -- ERROR: {4;1} (B3)
   RX07 : R0 renames RR2.F.F7;                -- ERROR: {4;1} (B3)
   RX08 : Integer renames RR2.F.F2.F;         -- ERROR: {4;1} (AB1)
   RX09 : Integer renames RR2.F.F3(1);        -- ERROR: {4;1} (AB2)
   RX10 : Integer renames RR2.F.F7.F;         -- ERROR: {4;1} (AB3)
   RX11 : R0 renames LRR2.F.F2;               -- ERROR: {4;1} (B1) -- We're
   RX12 : A0 renames LRR2.F.F3;               -- ERROR: {4;1} (B2) -- testing
   RX13 : R0 renames LRR2.F.F7;               -- ERROR: {4;1} (B3) -- F, not
                                                                   -- LRR2!
   RX14 : R0 renames Rec2'(RR1).F2;           -- ERROR: {4;1} (BH1)
   RX15 : A0 renames Rec2'(RR2).F3;           -- ERROR: {4;1} (BH2)
   RX15 : Integer renames Rec2'(RR1).F4;      -- ERROR: {4;1} (BH3)

   type RecArr is array (1..2) of Rec;

   A : RecArr := ((D => 1, F1 => 12, F2 => (1,1), F3 => (1 => 1),
                   F4 => -10, F5 => (1,2,3,4,5)),
                  (2, 10, (2,2), (1,2), 2.71, (2,2)));

   AX01 : Integer renames A(1).F1;            -- OK.
   AX02 : R0 renames A(1).F2;                 -- ERROR: {4;1} (B1)
   AX03 : A0 renames A(1).F3;                 -- ERROR: {4;1} (B2)
   AX04 : Integer renames A(1).F4;            -- ERROR: {4;1} (B3)
   AX05 : A0 renames A(1).F5;                 -- ERROR: {4;1} (B3)
   AX06 : Float renames A(2).F6;              -- ERROR: {4;1} (B3)
   AX07 : R0 renames A(2).F7;                 -- ERROR: {4;1} (B3)
   AX08 : Integer renames A(2).F2.F;          -- ERROR: {4;1} (AB3)
   AX09 : Integer renames A(2).F3(1);         -- ERROR: {4;1} (AB3)
   AX10 : Integer renames A(2).F7.F;          -- ERROR: {4;1} (AB3)
   AX11 : R0 renames Rec'(A(1)).F2;           -- ERROR: {4;1} (BH1)
   AX12 : A0 renames Rec'(A(2)).F3;           -- ERROR: {4;1} (BH2)
   AX13 : Integer renames Rec'(A(1)).F4;      -- ERROR: {4;1} (BH3)

   task Fooey is
      entry Ent1 (TR1 : out Rec);
      entry Ent2 (TR2 : in out Rec);
      entry Ent3 (TR3 : in Rec);
   end Fooey;

   protected type Locky is
      procedure P1 (QR1 : out Rec);
      entry P2 (QR2 : in out Rec);
   end Locky;

   procedure Proc (PR1 : out Rec; PR2 : in out Rec) is
      PX01 : Integer renames PR1.F1;          -- OK. {7;1} (C, not DDC)
      PX02 : R0 renames PR1.F2;               -- ERROR: {7;1} (C1)
      PX03 : A0 renames PR1.F3;               -- ERROR: {7;1} (C2)
      PX04 : Integer renames PR1.F4;          -- ERROR: {7;1} (C3)
      PX05 : A0 renames PR1.F5;               -- ERROR: {7;1} (C3)
      PX06 : R0 renames PR2.F2;               -- ERROR: {7;1} (C1)
      PX07 : A0 renames PR2.F3;               -- ERROR: {7;1} (C2)
      PX08 : Float renames PR2.F6;            -- ERROR: {7;1} (C3)
      PX09 : R0 renames PR2.F7;               -- ERROR: {7;1} (C3)
      PX10 : Integer renames PR2.F2.F;        -- ERROR: {7;1} (AC1)
      PX11 : Integer renames PR2.F3(1);       -- ERROR: {7;1} (AC2)
      PX12 : Integer renames PR2.F7.F;        -- ERROR: {7;1} (AC3)
      PX13 : R0 renames Rec'(PR1).F2;         -- ERROR: {7;1} (CH1)
      PX14 : A0 renames Rec'(PR2).F3;         -- ERROR: {7;1} (CH2)
      PX15 : Integer renames Rec'(PR1).F4;    -- ERROR: {7;1} (CH3)
   begin
        null;
   end Proc;

   task body Fooey is
   begin
      accept Ent1 (TR1 : out Rec) do
         declare
            TX01 : Integer renames TR1.F1;    -- OK. {13;1} (C, not DDC)
            TX02 : R0 renames TR1.F2;         -- ERROR: {13;1} (C1)
            TX03 : A0 renames TR1.F3;         -- ERROR: {13;1} (C2)
            TX04 : Integer renames TR1.F4;    -- ERROR: {13;1} (C3)
            TX05 : A0 renames TR1.F5;         -- ERROR: {13;1} (C3)
            TX13 : R0 renames Rec'(TR1).F2;   -- ERROR: {13;1} (CH1)
            TX15 : Integer renames Rec'(TR1).F4; -- ERROR: {13;1} (CH3)
         begin
            null;
         end;
      end Ent1;

      accept Ent2 (TR2 : in out Rec) do
         declare
            TX06 : R0 renames TR2.F2;         -- ERROR: {13;1} (C1)
            TX07 : A0 renames TR2.F3;         -- ERROR: {13;1} (C2)
            TX08 : Float renames TR2.F6;      -- ERROR: {13;1} (C3)
            TX09 : R0 renames TR2.F7;         -- ERROR: {13;1} (C3)
            TX10 : Integer renames TR2.F2.F;  -- ERROR: {13;1} (AC1)
            TX11 : Integer renames TR2.F3(1); -- ERROR: {13;1} (AC2)
            TX12 : Integer renames TR2.F7.F;  -- ERROR: {13;1} (AC3)
            TX13 : R0 renames Rec'(TR2).F2;   -- ERROR: {13;1} (CH1)
            TX14 : A0 renames Rec'(TR2).F3;   -- ERROR: {13;1} (CH2)
         begin
            null;
         end;
      end Ent2;

      accept Ent3 (TR3 : in Rec) do
         declare
            TI06 : R0 renames TR3.F2;         -- OK. {13;1} (V1)
            TI07 : A0 renames TR3.F3;         -- OK. {13;1} (V2)
            TI08 : Float renames TR3.F6;      -- OK. {13;1} (V3)
            TI09 : R0 renames TR3.F7;         -- OK. {13;1} (V3)
            TI10 : Integer renames TR3.F2.F;  -- OK. {13;1} (AV1)
            TI11 : Integer renames TR3.F3(1); -- OK. {13;1} (AV2)
            TI12 : Integer renames TR3.F7.F;  -- OK. {13;1} (AV3)
            TI13 : R0 renames Rec'(TR3).F2;   -- OK. {13;1} (VH1)
            TI14 : A0 renames Rec'(TR3).F3;   -- OK. {13;1} (VH2)
         begin
            null;
         end;
      end Ent3;
   end Fooey;

   protected body Locky is
      procedure P1 (QR1 : out Rec) is
         QX01 : Integer renames QR1.F1;    -- OK. {10;1} (C, not DDC)
         QX02 : R0 renames QR1.F2;         -- ERROR: {10;1} (C1)
         QX03 : A0 renames QR1.F3;         -- ERROR: {10;1} (C2)
         QX04 : Integer renames QR1.F4;    -- ERROR: {10;1} (C3)
         QX05 : A0 renames QR1.F5;         -- ERROR: {10;1} (C3)
         QX13 : R0 renames Rec'(QR1).F2;   -- ERROR: {10;1} (CH1)
         QX15 : Integer renames Rec'(QR1).F4; -- ERROR: {10;1} (CH3)
      begin
         null;
      end P1;

      entry P2 (QR2 : in out Rec) when True is
         QX06 : R0 renames QR2.F2;         -- ERROR: {10;1} (C1)
         QX07 : A0 renames QR2.F3;         -- ERROR: {10;1} (C2)
         QX08 : Float renames QR2.F6;      -- ERROR: {10;1} (C3)
         QX09 : R0 renames QR2.F7;         -- ERROR: {10;1} (C3)
         QX10 : Integer renames QR2.F2.F;  -- ERROR: {10;1} (AC1)
         QX11 : Integer renames QR2.F3(1); -- ERROR: {10;1} (AC2)
         QX12 : Integer renames QR2.F7.F;  -- ERROR: {10;1} (AC3)
         QX13 : R0 renames Rec'(QR2).F2;   -- ERROR: {10;1} (CH1)
         QX14 : A0 renames Rec'(QR2).F3;   -- ERROR: {10;1} (CH2)
      begin
         null;
      end P2;
   end Locky;


   function Funky (D : in Integer) return Rec is
   begin
      case D is
         when 1 =>
            return FR1 : Rec := R1 do
               declare
                  FX01 : Integer renames FR1.F1;    -- OK. {19;1} (E, not DDC)
                  FX02 : R0 renames FR1.F2;         -- ERROR: {19;1} (E1)
                  FX03 : A0 renames FR1.F3;         -- ERROR: {19;1} (E2)
                  FX04 : Integer renames FR1.F4;    -- ERROR: {19;1} (E3)
                  FX05 : A0 renames FR1.F5;         -- ERROR: {19;1} (E3)
                  FX13 : R0 renames Rec'(FR1).F2;   -- ERROR: {19;1} (EH1)
                  FX15 : Integer renames Rec'(FR1).F4; -- ERROR: {19;1} (EH3)
               begin
                  null;
               end;
            end return;
         when 2 =>
            return FR2 : Rec := R2 do
               declare
                  FX06 : R0 renames FR2.F2;         -- ERROR: {19;1} (E1)
                  FX07 : A0 renames FR2.F3;         -- ERROR: {19;1} (E2)
                  FX08 : Float renames FR2.F6;      -- ERROR: {19;1} (E3)
                  FX09 : R0 renames FR2.F7;         -- ERROR: {19;1} (E3)
                  FX10 : Integer renames FR2.F2.F;  -- ERROR: {19;1} (AE1)
                  FX11 : Integer renames FR2.F3(1); -- ERROR: {19;1} (AE2)
                  FX12 : Integer renames FR2.F7.F;  -- ERROR: {19;1} (AE3)
                  FX13 : R0 renames Rec'(FR2).F2;   -- ERROR: {19;1} (EH1)
                  FX14 : A0 renames Rec'(FR2).F3;   -- ERROR: {19;1} (EH2)
               begin
                  null;
               end;
            end return;
         when 3 =>
            return CFR2 : constant Rec := R2 do
               declare
                  CFX01 : Integer renames CFR2.F1;   -- OK. {19;1} (W, not DDC)
                  CFX06 : R0 renames CFR2.F2;        -- OK. {19;1} (W1)
                  CFX07 : A0 renames CFR2.F3;        -- OK. {19;1} (W2)
                  CFX08 : Float renames CFR2.F6;     -- OK. {19;1} (W3)
                  CFX09 : R0 renames CFR2.F7;        -- OK. {19;1} (W3)
                  CFX10 : Integer renames CFR2.F2.F; -- OK. {19;1} (AW1)
                  CFX11 : Integer renames CFR2.F3(1);-- OK. {19;1} (AW2)
                  CFX12 : Integer renames CFR2.F7.F; -- OK. {19;1} (AW3)
                  CFX13 : R0 renames Rec'(CFR2).F2;  -- OK. {19;1} (WH1)
                  CFX14 : A0 renames Rec'(CFR2).F3;  -- OK. {19;1} (WH2)
               begin
                  null;
               end;
            end return;
          when others => null;
      end case;
   end Funky;

begin
   null;
end B851005;
